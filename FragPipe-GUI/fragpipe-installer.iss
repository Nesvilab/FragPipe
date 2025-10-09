#define AppName "FragPipe"
#define AppVersion "23.2-build17"
#define AppPublisher "Nesvizhskii Lab"
#define AppURL "https://fragpipe.nesvilab.org/"
#define AppExeName AppName + "-" + AppVersion + ".exe"
#define SourcePath "build/github-release/" + AppName + "-" + AppVersion + "-windows/fragpipe-" + AppVersion
#define DefaultInstallPath "C:\" + AppName + "\" + AppName
#define LicenseFile "../LICENSE"

[Setup]
; NOTE: The value of AppId uniquely identifies this application. Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{715C6F6E-5626-4A8E-80C1-C8BF2BBB8753}}-{#AppVersion}
AppName={#AppName}
AppVersion={#AppVersion}
;AppVerName={#AppName} {#AppVersion}
AppPublisher={#AppPublisher}
AppPublisherURL={#AppURL}
AppSupportURL={#AppURL}
AppUpdatesURL={#AppURL}
DefaultDirName={#DefaultInstallPath}-{#AppVersion}
UsePreviousAppDir=no
UninstallDisplayIcon={app}\bin\{#AppExeName}
DisableProgramGroupPage=yes
DisableDirPage=no
; Remove the following line to run in administrative install mode (install for all users).
PrivilegesRequired=lowest
OutputBaseFilename={#AppName}-{#AppVersion}-installer
OutputDir=build/installer
SolidCompression=yes
WizardStyle=modern
LicenseFile={#LicenseFile}

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"

[Files]
Source: "{#SourcePath}\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{autoprograms}\{#AppExeName}"; Filename: "{app}\bin\{#AppExeName}"
Name: "{autodesktop}\{#AppExeName}"; Filename: "{app}\bin\{#AppExeName}"; Tasks: desktopicon

[UninstallDelete]
Type: filesandordirs; Name: "{app}"

[Run]
Filename: "cmd.exe"; Parameters: "/C set PATH={app}\python;%PATH% && {app}\python_packages\uv pip install --system --no-cache --no-deps -r python_packages\requirements.txt --no-index --find-links   python_packages"; WorkingDir: "{app}"; Flags: waituntilterminated

Filename: "{app}\bin\{#AppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(AppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent