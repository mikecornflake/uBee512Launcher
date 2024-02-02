Unit DialogSettings;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  EditBtn, StdCtrls, inifiles;

Type

  { TSettings }

  TSettings = Class(TPersistent)
  Public
    UBEE512_exe: String;
    UBEE512_rc: String;
    RUNCPM_exe: String;
    CPMTOOLS_bin: String;
    WorkingFolder: String;

    A, B, C: String;
    A_Format, B_Format, C_Format: String;

    Procedure ValidatePaths;
    Procedure InitialisePaths;

    Procedure Assign(Source: TPersistent); Override;

    Procedure LoadSettings(AInifile: TInifile);
    Procedure SaveSettings(AInifile: TInifile);

    Function Folder: String;
  End;

  { TdlgSettings }

  TdlgSettings = Class(TForm)
    btnRescan: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnSettingsFolder: TButton;
    edtCPMTools: TDirectoryEdit;
    edtRunCPM: TFileNameEdit;
    edtuBee512exe: TFileNameEdit;
    edtuBee512rc: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    tsLocations: TTabSheet;
    Procedure btnOKClick(Sender: TObject);
    Procedure btnSettingsFolderClick(Sender: TObject);
    Procedure btnRescanClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  Private
    FActivated: Boolean;
    FSettings: TSettings;

    Procedure LoadControls;
    Procedure SetSettings(AValue: TSettings);
  Public
    Property Settings: TSettings read FSettings write SetSettings;
  End;

Implementation

Uses
  uBee512Support, cpmtoolsSupport, FileSupport, Logs, LCLIntf;

{$R *.lfm}

{ TdlgSettings }

Procedure TdlgSettings.FormCreate(Sender: TObject);
Begin
  FSettings := TSettings.Create;
  FActivated := False;
End;

Procedure TdlgSettings.FormActivate(Sender: TObject);
Begin
  If Not FActivated Then
  Begin
    MakeFullyVisible;

    LoadControls;

    FActivated := True;
  End;
End;

Procedure TdlgSettings.btnOKClick(Sender: TObject);
Begin
  FSettings.UBEE512_exe := edtuBee512exe.Text;
  FSettings.UBEE512_rc := edtuBee512rc.Text;
  FSettings.RUNCPM_exe := edtRunCPM.Text;
  FSettings.CPMTOOLS_bin := edtCPMTools.Text;

  ModalResult := mrOk;
End;

Procedure TdlgSettings.btnSettingsFolderClick(Sender: TObject);
Begin
  OpenDocument(FSettings.Folder);
End;

Procedure TdlgSettings.btnRescanClick(Sender: TObject);
Begin
  Debug('TfrmSettings.btnRescanClick');

  FSettings.InitialisePaths;

  LoadControls;
End;

Procedure TdlgSettings.SetSettings(AValue: TSettings);
Begin
  Assert(Assigned(FSettings), 'TfrmSettings.FSetting not assigned');

  If Assigned(AValue) Then
    FSettings.Assign(AValue);
End;

Procedure TdlgSettings.LoadControls;
Begin
  edtuBee512exe.Text := FSettings.UBEE512_exe;
  edtuBee512rc.Text := FSettings.UBEE512_rc;
  edtRunCPM.Text := FSettings.RUNCPM_exe;
  edtCPMTools.Text := FSettings.CPMTOOLS_bin;
End;

{ TSettings }

Procedure TSettings.Assign(Source: TPersistent);
Begin
  If Assigned(Source) And (Source Is TSettings) Then
  Begin
    UBEE512_exe := TSettings(Source).UBEE512_exe;
    UBEE512_rc := TSettings(Source).UBEE512_rc;
    RUNCPM_exe := TSettings(Source).RUNCPM_exe;
    CPMTOOLS_bin := TSettings(Source).CPMTOOLS_bin;
    WorkingFolder := TSettings(Source).WorkingFolder;

    A := TSettings(Source).A;
    B := TSettings(Source).B;
    C := TSettings(Source).C;

    A_Format := TSettings(Source).A_Format;
    B_Format := TSettings(Source).B_Format;
    C_Format := TSettings(Source).C_Format;
  End;
End;

Procedure TSettings.LoadSettings(AInifile: TInifile);
Begin
  InitialisePaths;

  UBEE512_exe := AInifile.ReadString('Locations', 'uBee512exe', uBee512.exe);
  UBEE512_rc := AInifile.ReadString('Locations', 'uBee512rc', uBee512.rc);
  RUNCPM_exe := AInifile.ReadString('Locations', 'RunCPM', '');
  CPMTOOLS_bin := AInifile.ReadString('Locations', 'cpmtools', '');
  WorkingFolder := ExcludeSlash(AInifile.ReadString('Locations', 'Working',
    ExtractFilePath(Application.ExeName)));

  A := AInifile.ReadString('Selected', 'Disk A', '');
  B := AInifile.ReadString('Selected', 'Disk B', '');
  C := AInifile.ReadString('Selected', 'Disk C', '');

  A_Format := AInifile.ReadString('Selected', 'Disk A Format', '');
  B_Format := AInifile.ReadString('Selected', 'Disk B Format', '');
  C_Format := AInifile.ReadString('Selected', 'Disk C Format', '');

  ValidatePaths;

  Debug('TSettings.LoadSettings uBee512exe=' + uBee512.exe);
  Debug('TSettings.LoadSettings uBee512rc=' + uBee512.rc);
  Debug('TSettings.LoadSettings cpmtools=' + cpmtoolsPath);
End;

Procedure TSettings.SaveSettings(AInifile: TInifile);
Begin
  AInifile.WriteString('Locations', 'uBee512exe', UBEE512_exe);
  AInifile.WriteString('Locations', 'uBee512rc', UBEE512_rc);
  AInifile.WriteString('Locations', 'RunCPM', RUNCPM_exe);
  AInifile.WriteString('Locations', 'cpmtools', CPMTOOLS_bin);
  AInifile.WriteString('Locations', 'Working', WorkingFolder);

  AInifile.WriteString('Selected', 'Disk A', A);
  AInifile.WriteString('Selected', 'Disk B', B);
  AInifile.WriteString('Selected', 'Disk C', C);

  AInifile.WriteString('Selected', 'Disk A Format', A_Format);
  AInifile.WriteString('Selected', 'Disk B Format', B_Format);
  AInifile.WriteString('Selected', 'Disk C Format', C_Format);
End;

Function TSettings.Folder: String;
Begin
  Result := ExcludeSlash(GetAppConfigDir(False));
  If Not DirectoryExists(Result) Then
    ForceDirectories(Result);
End;

Procedure TSettings.InitialisePaths;
Begin
  // Don't call this unnecessarily.  it'll reset any folders the user has set up.
  // Use this only on first run (ie before load ini file)
  Initializecpmtools;
  uBee512.Initialize;

  // One of these will be right
  CPMTOOLS_bin := cpmtoolsPath;

  UBEE512_exe := uBee512.exe;
  UBEE512_rc := uBee512.rc;

  Debug('TSettings.InitialisePaths uBee512exe=' + uBee512.exe);
  Debug('TSettings.InitialisePaths uBee512rc=' + uBee512.rc);
  Debug('TSettings.InitialisePaths cpmtools=' + cpmtoolsPath);
End;

Procedure TSettings.ValidatePaths;
Begin
  If DirectoryExists(CPMTOOLS_bin) Then
    SetcpmtoolsPath(CPMTOOLS_bin)
  Else
    CPMTOOLS_bin := '';

  If FileExists(UBEE512_exe) Then
    uBee512.exe := UBEE512_exe
  Else
    UBEE512_exe := '';

  If FileExists(UBEE512_rc) Then
    uBee512.rc := UBEE512_rc
  Else
    UBEE512_rc := '';

  If Not DirectoryExists(WorkingFolder) Then
    WorkingFolder := ExtractFileDir(uBee512.rc);

  If Not FileExists(RUNCPM_exe) Then
    RUNCPM_exe := '';

  Debug('TSettings.ValidatePaths uBee512exe=' + uBee512.exe);
  Debug('TSettings.ValidatePaths uBee512rc=' + uBee512.rc);
  Debug('TSettings.ValidatePaths cpmtools=' + cpmtoolsPath);
End;

End.
