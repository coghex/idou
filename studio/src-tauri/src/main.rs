#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use std::collections::VecDeque;
use std::fs;
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use rfd::FileDialog;
use serde::{Deserialize, Serialize};
use tauri::State;

#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
enum TransportState {
    #[default]
    Stopped,
    Playing,
    Paused,
}

#[derive(Default)]
struct SharedState {
    playback: Arc<Mutex<Option<ManagedPlayback>>>,
    audition: Arc<Mutex<Option<ManagedAudition>>>,
    logs: Arc<Mutex<VecDeque<String>>>,
}

struct ManagedPlayback {
    child: Child,
    staged_path: PathBuf,
    transport_state: TransportState,
    loop_section_name: Option<String>,
}

struct ManagedAudition {
    child: Child,
}

#[derive(Serialize)]
struct FilePayload {
    path: String,
    contents: String,
}

#[derive(Deserialize)]
struct SaveSongRequest {
    path: Option<String>,
    contents: String,
}

#[derive(Serialize)]
struct SaveSongResponse {
    path: String,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct PlaySongRequest {
    contents: String,
    suggested_name: Option<String>,
    loop_section_name: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct TimelineDumpRequest {
    contents: String,
    suggested_name: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct PreviewNoteRequest {
    genre: String,
    instrument_name: String,
    midi: i32,
}

#[derive(Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
struct TimelinePreview {
    sections: Vec<TimelinePreviewSection>,
}

#[derive(Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
struct TimelinePreviewSection {
    name: String,
    beats_per_bar: i32,
    total_beats: f64,
    notes: Vec<TimelinePreviewNote>,
}

#[derive(Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
struct TimelinePreviewNote {
    instrument_id: i32,
    instrument_name: String,
    key: i32,
    velocity: f64,
    amp: f64,
    pan: f64,
    bar_index: i32,
    start_beat: f64,
    duration_beats: f64,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct PlaybackSnapshot {
    running: bool,
    transport_state: TransportState,
    staged_path: Option<String>,
    loop_section_name: Option<String>,
    log_lines: Vec<String>,
}

fn push_log(logs: &Arc<Mutex<VecDeque<String>>>, line: impl Into<String>) {
    let mut guard = logs.lock().expect("log mutex poisoned");
    guard.push_back(line.into());
    while guard.len() > 400 {
        guard.pop_front();
    }
}

fn repo_root() -> Result<PathBuf, String> {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .join("..")
        .join("..")
        .canonicalize()
        .map_err(|error| format!("Failed to resolve repo root: {error}"))
}

fn preview_path(suggested_name: Option<&str>) -> PathBuf {
    let safe_name = suggested_name
        .unwrap_or("preview")
        .chars()
        .filter(|ch| ch.is_ascii_alphanumeric() || *ch == '-')
        .collect::<String>();
    let name = if safe_name.is_empty() {
        "preview"
    } else {
        &safe_name
    };
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis();
    std::env::temp_dir().join(format!("idou-studio-{name}-{millis}.yaml"))
}

fn remove_staged_file(path: &Path, logs: &Arc<Mutex<VecDeque<String>>>) {
    if let Err(error) = fs::remove_file(path) {
        if error.kind() != std::io::ErrorKind::NotFound {
            push_log(
                logs,
                format!(
                    "Failed to remove staged preview file {}: {error}",
                    path.display()
                ),
            );
        }
    }
}

fn handle_process_line(
    playback: &Arc<Mutex<Option<ManagedPlayback>>>,
    logs: &Arc<Mutex<VecDeque<String>>>,
    line: &str,
) {
    let mut loop_restart = false;
    let mut loop_target = None;

    if let Ok(mut guard) = playback.lock() {
        if let Some(managed) = guard.as_mut() {
            if line.contains("[player] timeline started") {
                managed.transport_state = TransportState::Playing;
            } else if line.contains("[player] timeline stopped") {
                if managed.transport_state == TransportState::Playing {
                    managed.transport_state = TransportState::Paused;
                }
            } else if line.contains("[player] timeline finished") {
                if let Some(section_name) = managed.loop_section_name.clone() {
                    if let Some(stdin) = managed.child.stdin.as_mut() {
                        if stdin
                            .write_all(b"start\n")
                            .and_then(|_| stdin.flush())
                            .is_ok()
                        {
                            managed.transport_state = TransportState::Playing;
                            loop_restart = true;
                            loop_target = Some(section_name);
                        } else {
                            managed.transport_state = TransportState::Stopped;
                        }
                    } else {
                        managed.transport_state = TransportState::Stopped;
                    }
                } else {
                    managed.transport_state = TransportState::Stopped;
                }
            } else if line.contains("[player] timeline load failed") {
                managed.transport_state = TransportState::Stopped;
            }
        }
    }

    if loop_restart {
        if let Some(section_name) = loop_target {
            push_log(
                logs,
                format!("[studio] Restarting loop for section {section_name}."),
            );
        }
    }
}

fn spawn_log_reader<T: std::io::Read + Send + 'static>(
    reader: T,
    label: &'static str,
    playback: Arc<Mutex<Option<ManagedPlayback>>>,
    logs: Arc<Mutex<VecDeque<String>>>,
) {
    thread::spawn(move || {
        for line in BufReader::new(reader).lines() {
            match line {
                Ok(line) => {
                    push_log(&logs, format!("[{label}] {line}"));
                    if label == "stdout" {
                        handle_process_line(&playback, &logs, &line);
                    }
                }
                Err(error) => {
                    push_log(&logs, format!("[{label}] log stream error: {error}"));
                    break;
                }
            }
        }
    });
}

fn spawn_plain_log_reader<T: std::io::Read + Send + 'static>(
    reader: T,
    label: &'static str,
    logs: Arc<Mutex<VecDeque<String>>>,
) {
    thread::spawn(move || {
        for line in BufReader::new(reader).lines() {
            match line {
                Ok(line) => push_log(&logs, format!("[{label}] {line}")),
                Err(error) => {
                    push_log(&logs, format!("[{label}] log stream error: {error}"));
                    break;
                }
            }
        }
    });
}

fn stop_playback_internal(state: &SharedState) -> Result<(), String> {
    let mut guard = state
        .playback
        .lock()
        .map_err(|_| "Playback state lock poisoned.".to_string())?;
    let Some(mut managed) = guard.take() else {
        return Ok(());
    };

    let pid = managed.child.id();
    push_log(&state.logs, format!("Stopping preview process {pid}."));
    if let Some(stdin) = managed.child.stdin.as_mut() {
        let _ = stdin.write_all(b"q\n");
        let _ = stdin.flush();
    }

    let mut exited = false;
    for _ in 0..10 {
        match managed.child.try_wait() {
            Ok(Some(status)) => {
                push_log(
                    &state.logs,
                    format!("Preview process exited with {status}."),
                );
                exited = true;
                break;
            }
            Ok(None) => thread::sleep(Duration::from_millis(100)),
            Err(error) => return Err(format!("Failed to poll preview process: {error}")),
        }
    }

    if !exited {
        let _ = managed.child.kill();
        let _ = managed.child.wait();
        push_log(&state.logs, format!("Force-stopped preview process {pid}."));
    }

    remove_staged_file(&managed.staged_path, &state.logs);
    Ok(())
}

fn playback_snapshot_internal(state: &SharedState) -> Result<PlaybackSnapshot, String> {
    let mut staged_path = None;
    let mut running = false;
    let mut transport_state = TransportState::Stopped;
    let mut loop_section_name = None;
    let mut finished_path: Option<PathBuf> = None;

    {
        let mut guard = state
            .playback
            .lock()
            .map_err(|_| "Playback state lock poisoned.".to_string())?;

        if let Some(mut managed) = guard.take() {
            match managed.child.try_wait() {
                Ok(Some(status)) => {
                    push_log(
                        &state.logs,
                        format!("Preview process exited with {status}."),
                    );
                    finished_path = Some(managed.staged_path);
                }
                Ok(None) => {
                    staged_path = Some(managed.staged_path.display().to_string());
                    transport_state = managed.transport_state;
                    loop_section_name = managed.loop_section_name.clone();
                    running = true;
                    *guard = Some(managed);
                }
                Err(error) => {
                    *guard = Some(managed);
                    return Err(format!("Failed to poll preview process: {error}"));
                }
            }
        }
    }

    if let Some(path) = finished_path {
        remove_staged_file(&path, &state.logs);
    }

    let log_lines = state
        .logs
        .lock()
        .map_err(|_| "Log state lock poisoned.".to_string())?
        .iter()
        .cloned()
        .collect::<Vec<_>>();

    Ok(PlaybackSnapshot {
        running,
        transport_state,
        staged_path,
        loop_section_name,
        log_lines,
    })
}

fn send_transport_command(
    state: &SharedState,
    command: &str,
    next_state: TransportState,
) -> Result<PlaybackSnapshot, String> {
    let mut guard = state
        .playback
        .lock()
        .map_err(|_| "Playback state lock poisoned.".to_string())?;

    let Some(managed) = guard.as_mut() else {
        return Err("No preview process is running.".to_string());
    };

    match managed.child.try_wait() {
        Ok(Some(status)) => {
            let staged_path = managed.staged_path.clone();
            guard.take();
            drop(guard);
            push_log(
                &state.logs,
                format!("Preview process exited with {status}."),
            );
            remove_staged_file(&staged_path, &state.logs);
            Err("Preview process is no longer running.".to_string())
        }
        Ok(None) => {
            if let Some(stdin) = managed.child.stdin.as_mut() {
                stdin
                    .write_all(command.as_bytes())
                    .and_then(|_| stdin.flush())
                    .map_err(|error| {
                        format!(
                            "Failed to send transport command `{}`: {error}",
                            command.trim()
                        )
                    })?;
                managed.transport_state = next_state;
                push_log(
                    &state.logs,
                    format!("[studio] Sent transport command `{}`.", command.trim()),
                );
                drop(guard);
                playback_snapshot_internal(state)
            } else {
                Err("Preview process stdin is unavailable.".to_string())
            }
        }
        Err(error) => Err(format!("Failed to poll preview process: {error}")),
    }
}

fn ensure_note_preview_process(state: &SharedState) -> Result<(), String> {
    {
        let mut guard = state
            .audition
            .lock()
            .map_err(|_| "Note preview state lock poisoned.".to_string())?;

        if let Some(mut managed) = guard.take() {
            match managed.child.try_wait() {
                Ok(Some(status)) => {
                    push_log(
                        &state.logs,
                        format!("Note preview process exited with {status}. Restarting."),
                    );
                }
                Ok(None) => {
                    *guard = Some(managed);
                    return Ok(());
                }
                Err(error) => {
                    return Err(format!("Failed to poll note preview process: {error}"));
                }
            }
        }
    }

    let repo_root = repo_root()?;
    push_log(
        &state.logs,
        format!("Launching note preview engine from {}.", repo_root.display()),
    );

    let mut command = Command::new("cabal");
    command
        .current_dir(&repo_root)
        .arg("run")
        .arg("idou")
        .arg("--")
        .arg("--note-preview-shell")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = command
        .spawn()
        .map_err(|error| format!("Failed to launch note preview engine: {error}"))?;

    if let Some(stdout) = child.stdout.take() {
        spawn_plain_log_reader(stdout, "preview-stdout", state.logs.clone());
    }
    if let Some(stderr) = child.stderr.take() {
        spawn_plain_log_reader(stderr, "preview-stderr", state.logs.clone());
    }

    let pid = child.id();
    push_log(
        &state.logs,
        format!("Note preview process started with pid {pid}."),
    );

    let mut guard = state
        .audition
        .lock()
        .map_err(|_| "Note preview state lock poisoned.".to_string())?;
    *guard = Some(ManagedAudition { child });
    Ok(())
}

#[tauri::command]
fn preview_song_note(request: PreviewNoteRequest, state: State<'_, SharedState>) -> Result<(), String> {
    ensure_note_preview_process(&state)?;

    let mut guard = state
        .audition
        .lock()
        .map_err(|_| "Note preview state lock poisoned.".to_string())?;
    let Some(managed) = guard.as_mut() else {
        return Err("Note preview process is unavailable.".to_string());
    };

    match managed.child.try_wait() {
        Ok(Some(status)) => {
            guard.take();
            push_log(
                &state.logs,
                format!("Note preview process exited with {status}."),
            );
            Err("Note preview process is no longer running.".to_string())
        }
        Ok(None) => {
            let command = format!(
                "note {} {} {}\n",
                request.genre.trim(),
                request.instrument_name.trim(),
                request.midi
            );
            if let Some(stdin) = managed.child.stdin.as_mut() {
                stdin
                    .write_all(command.as_bytes())
                    .and_then(|_| stdin.flush())
                    .map_err(|error| format!("Failed to send note preview command: {error}"))?;
                push_log(
                    &state.logs,
                    format!(
                        "[studio] Audition note {} on {} ({})",
                        request.midi, request.instrument_name, request.genre
                    ),
                );
                Ok(())
            } else {
                Err("Note preview process stdin is unavailable.".to_string())
            }
        }
        Err(error) => Err(format!("Failed to poll note preview process: {error}")),
    }
}

#[tauri::command]
fn open_song_file() -> Result<Option<FilePayload>, String> {
    let Some(path) = FileDialog::new()
        .add_filter("Idou song", &["yaml", "yml"])
        .set_title("Open Idou song YAML")
        .pick_file()
    else {
        return Ok(None);
    };

    let contents = fs::read_to_string(&path)
        .map_err(|error| format!("Failed to read {}: {error}", path.display()))?;

    Ok(Some(FilePayload {
        path: path.display().to_string(),
        contents,
    }))
}

#[tauri::command]
fn save_song_file(request: SaveSongRequest) -> Result<Option<SaveSongResponse>, String> {
    let path = if let Some(path) = request.path {
        PathBuf::from(path)
    } else {
        let Some(path) = FileDialog::new()
            .add_filter("Idou song", &["yaml", "yml"])
            .set_file_name("song.yaml")
            .set_title("Save Idou song YAML")
            .save_file()
        else {
            return Ok(None);
        };
        path
    };

    fs::write(&path, request.contents)
        .map_err(|error| format!("Failed to write {}: {error}", path.display()))?;

    Ok(Some(SaveSongResponse {
        path: path.display().to_string(),
    }))
}

#[tauri::command]
fn play_song_preview(
    request: PlaySongRequest,
    state: State<'_, SharedState>,
) -> Result<PlaybackSnapshot, String> {
    stop_playback_internal(&state)?;

    {
        let mut logs = state
            .logs
            .lock()
            .map_err(|_| "Log state lock poisoned.".to_string())?;
        logs.clear();
    }

    let repo_root = repo_root()?;
    let staged_path = preview_path(request.suggested_name.as_deref());
    fs::write(&staged_path, request.contents).map_err(|error| {
        format!(
            "Failed to write staged preview YAML {}: {error}",
            staged_path.display()
        )
    })?;

    push_log(
        &state.logs,
        format!("Staged preview YAML at {}.", staged_path.display()),
    );
    push_log(
        &state.logs,
        format!("Launching Idou engine from {}.", repo_root.display()),
    );

    let mut command = Command::new("cabal");
    command
        .current_dir(&repo_root)
        .arg("run")
        .arg("idou")
        .arg("--")
        .arg(&staged_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = command.spawn().map_err(|error| {
        format!(
            "Failed to launch `cabal run idou -- {}`: {error}",
            staged_path.display()
        )
    })?;

    if let Some(stdout) = child.stdout.take() {
        spawn_log_reader(stdout, "stdout", state.playback.clone(), state.logs.clone());
    }
    if let Some(stderr) = child.stderr.take() {
        spawn_log_reader(stderr, "stderr", state.playback.clone(), state.logs.clone());
    }

    let pid = child.id();
    push_log(
        &state.logs,
        format!("Preview process started with pid {pid}."),
    );

    let mut guard = state
        .playback
        .lock()
        .map_err(|_| "Playback state lock poisoned.".to_string())?;
    *guard = Some(ManagedPlayback {
        child,
        staged_path,
        transport_state: TransportState::Playing,
        loop_section_name: request.loop_section_name,
    });
    drop(guard);

    playback_snapshot_internal(&state)
}

#[tauri::command]
fn pause_song_preview(state: State<'_, SharedState>) -> Result<PlaybackSnapshot, String> {
    send_transport_command(&state, "stop\n", TransportState::Paused)
}

#[tauri::command]
fn resume_song_preview(state: State<'_, SharedState>) -> Result<PlaybackSnapshot, String> {
    send_transport_command(&state, "start\n", TransportState::Playing)
}

#[tauri::command]
fn stop_song_preview(state: State<'_, SharedState>) -> Result<PlaybackSnapshot, String> {
    stop_playback_internal(&state)?;
    playback_snapshot_internal(&state)
}

#[tauri::command]
fn playback_snapshot(state: State<'_, SharedState>) -> Result<PlaybackSnapshot, String> {
    playback_snapshot_internal(&state)
}

#[tauri::command]
fn dump_song_timeline(
    request: TimelineDumpRequest,
    state: State<'_, SharedState>,
) -> Result<TimelinePreview, String> {
    let repo_root = repo_root()?;
    let staged_path = preview_path(request.suggested_name.as_deref());
    fs::write(&staged_path, request.contents).map_err(|error| {
        format!(
            "Failed to write staged visualization YAML {}: {error}",
            staged_path.display()
        )
    })?;

    let output = Command::new("cabal")
        .current_dir(&repo_root)
        .arg("run")
        .arg("idou")
        .arg("--")
        .arg("--dump-timeline-json")
        .arg(&staged_path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|error| format!("Failed to launch timeline dump: {error}"))?;

    remove_staged_file(&staged_path, &state.logs);

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let message = if !stderr.is_empty() {
            stderr
        } else if !stdout.is_empty() {
            stdout
        } else {
            format!("Timeline dump failed with status {}.", output.status)
        };
        return Err(message);
    }

    serde_json::from_slice::<TimelinePreview>(&output.stdout)
        .map_err(|error| format!("Failed to parse timeline dump JSON: {error}"))
}

fn main() {
    tauri::Builder::default()
        .manage(SharedState::default())
        .invoke_handler(tauri::generate_handler![
            open_song_file,
            save_song_file,
            play_song_preview,
            pause_song_preview,
            resume_song_preview,
            stop_song_preview,
            playback_snapshot,
            dump_song_timeline,
            preview_song_note,
        ])
        .run(tauri::generate_context!())
        .expect("error while running Idou Studio");
}
