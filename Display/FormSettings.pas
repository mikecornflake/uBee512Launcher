Unit FormSettings;

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
    MOD_CPMTOOLS_bin: String;
    WorkingFolder: String;

    Procedure ValidatePaths;
    Procedure InitialisePaths;

    Procedure Assign(Source: TPersistent); Override;

    Procedure LoadSettings(AInifile: TInifile);
    Procedure SaveSettings(AInifile: TInifile);
  End;

  { TfrmSettings }

  TfrmSettings = Class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    edtCPMTools: TFileNameEdit;
    edtCPMToolsModified: TFileNameEdit;
    edtRunCPM: TFileNameEdit;
    edtuBee512exe: TFileNameEdit;
    edtuBee512rc: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    tsLocations: TTabSheet;
    Procedure btnOKClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  Private
    FActivated: Boolean;
    FSettings: TSettings;
    Procedure SetSettings(AValue: TSettings);

  Public
    Property Settings: TSettings read FSettings write SetSettings;
  End;

Implementation

Uses
  uBee512Support, cpmtoolsSupport, FileSupport, OSSupport;

{$R *.lfm}

{ TfrmSettings }

Procedure TfrmSettings.FormCreate(Sender: TObject);
Begin
  FSettings := TSettings.Create;
  FActivated := False;
End;

Procedure TfrmSettings.FormActivate(Sender: TObject);
Begin
  If Not FActivated Then
  Begin
    edtuBee512exe.Text := FSettings.UBEE512_exe;
    edtuBee512rc.Text := FSettings.UBEE512_rc;
    edtRunCPM.Text := FSettings.RUNCPM_exe;
    edtCPMTools.Text := FSettings.CPMTOOLS_bin;
    edtCPMToolsModified.Text := FSettings.MOD_CPMTOOLS_bin;

    FActivated := True;
  End;
End;

Procedure TfrmSettings.btnOKClick(Sender: TObject);
Begin
  FSettings.UBEE512_exe := edtuBee512exe.Text;
  FSettings.UBEE512_rc := edtuBee512rc.Text;
  FSettings.RUNCPM_exe := edtRunCPM.Text;
  FSettings.CPMTOOLS_bin := edtCPMTools.Text;
  FSettings.MOD_CPMTOOLS_bin := edtCPMToolsModified.Text;

  ModalResult := mrOk;
End;

Procedure TfrmSettings.SetSettings(AValue: TSettings);
Begin
  Assert(Assigned(FSettings), 'TfrmSettings.FSetting not assigned');

  If Assigned(AValue) Then
    FSettings.Assign(AValue);
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
    MOD_CPMTOOLS_bin := TSettings(Source).MOD_CPMTOOLS_bin;
    WorkingFolder := TSettings(Source).WorkingFolder;
  End;
End;

Procedure TSettings.LoadSettings(AInifile: TInifile);
Begin
  InitialisePaths;

  UBEE512_exe := AInifile.ReadString('Locations', 'uBee512exe', uBee512exe);
  UBEE512_rc := AInifile.ReadString('Locations', 'uBee512rc', uBee512rc);
  RUNCPM_exe := AInifile.ReadString('Locations', 'RunCPM', '');
  CPMTOOLS_bin := AInifile.ReadString('Locations', 'cpmtools', '');
  MOD_CPMTOOLS_bin := AInifile.ReadString('Locations', 'Modified cpmtools', cpmtoolsPath);
  WorkingFolder := IncludeSlash(AInifile.ReadString('Locations', 'Working',
    ExtractFilePath(Application.ExeName)));

  ValidatePaths;
End;

Procedure TSettings.SaveSettings(AInifile: TInifile);
Begin
  AInifile.WriteString('Locations', 'uBee512exe', UBEE512_exe);
  AInifile.WriteString('Locations', 'uBee512rc', UBEE512_rc);
  AInifile.WriteString('Locations', 'RunCPM', RUNCPM_exe);
  AInifile.WriteString('Locations', 'cpmtools', CPMTOOLS_bin);
  AInifile.WriteString('Locations', 'Modified cpmtools', MOD_CPMTOOLS_bin);
  AInifile.WriteString('Locations', 'Working', WorkingFolder);
End;

Procedure TSettings.InitialisePaths;
Begin
  // Don't call this unnecessarily.  it'll reset any folders the user has set up.
  // Use this only on first run (ie before load ini file)
  Initializecpmtools;
  InitializeuBee512;

  // One of these will be right
  CPMTOOLS_bin := cpmtoolsPath;
  MOD_CPMTOOLS_bin := cpmtoolsPath;

  UBEE512_exe := uBee512exe;
  UBEE512_rc := uBee512rc;
End;

Procedure TSettings.ValidatePaths;
Begin
  If DirectoryExists(CPMTOOLS_bin) Then
    SetcpmtoolsPath(CPMTOOLS_bin)
  Else
    CPMTOOLS_bin := '';

  // Looks like duplication of code above, but is ensuring cpmtools is using the
  // modified where available
  If DirectoryExists(MOD_CPMTOOLS_bin) Then
    SetcpmtoolsPath(MOD_CPMTOOLS_bin)
  Else
    MOD_CPMTOOLS_bin := '';

  If FileExists(UBEE512_exe) Then
    SetuBee512exe(UBEE512_exe)
  Else
    UBEE512_exe := '';

  If FileExists(UBEE512_rc) Then
    SetuBee512rc(UBEE512_rc)
  Else
    UBEE512_rc := '';

  If Not DirectoryExists(WorkingFolder) Then
    WorkingFolder := ExtractFileDir(uBee512rc);

  If Not FileExists(RUNCPM_exe) Then
    RUNCPM_exe := '';
End;

End.
