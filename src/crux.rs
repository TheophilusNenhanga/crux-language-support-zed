use zed_extension_api as zed;

struct CruxExtension;

impl zed::Extension for CruxExtension {
    fn new() -> Self {
        CruxExtension
    }

    fn language_server_command(
        &mut self,
        language_server_id: &zed::LanguageServerId,
        worktree: &zed::Worktree,
    ) -> zed::Result<zed::Command> {
        let path = worktree
            .which("crux-ls")
            .ok_or_else(|| "crux-ls not found in PATH".to_string())?;

        Ok(zed::Command {
            command: path,
            args: vec![],
            env: vec![],
        })
    }
}

zed::register_extension!(CruxExtension);
