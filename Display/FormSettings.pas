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
    RUNCPM_exe: String;
    CPMTOOLS_bin: String;
    MOD_CPMTOOLS_bin: String;
    WorkingFolder: String;

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
    edtuBee512: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListView1: TListView;
    memRC: TMemo;
    memSystems: TMemo;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    Splitter1: TSplitter;
    tsRC2: TTabSheet;
    TabSheet2: TTabSheet;
    tsRC: TTabSheet;
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
  uBee512Support, cpmtoolsSupport, FileSupport;

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
    edtuBee512.Text := FSettings.UBEE512_exe;
    edtRunCPM.Text := FSettings.RUNCPM_exe;
    edtCPMTools.Text := FSettings.CPMTOOLS_bin;
    edtCPMToolsModified.Text := FSettings.MOD_CPMTOOLS_bin;

    FActivated := True;
  End;
End;

Procedure TfrmSettings.btnOKClick(Sender: TObject);
Begin
  FSettings.UBEE512_exe := edtuBee512.Text;
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
    RUNCPM_exe := TSettings(Source).RUNCPM_exe;
    CPMTOOLS_bin := TSettings(Source).CPMTOOLS_bin;
    MOD_CPMTOOLS_bin := TSettings(Source).MOD_CPMTOOLS_bin;
    WorkingFolder := TSettings(Source).WorkingFolder;
  End;
End;

Procedure TSettings.LoadSettings(AInifile: TInifile);
Begin
  Initializecpmtools;
  InitializeuBee512;

  UBEE512_exe := AInifile.ReadString('Locations', 'uBee512',
    'B:\Drives\Microbee\ubee512\ubee512.exe');
  RUNCPM_exe := AInifile.ReadString('Locations', 'RunCPM', 'B:\Drives\CPM\RunCPM\RunCPM.exe');
  CPMTOOLS_bin := AInifile.ReadString('Locations', 'cpmtools', 'B:\Drives\CPM\cpmtools');
  MOD_CPMTOOLS_bin := AInifile.ReadString('Locations', 'Modified cpmtools',
    'B:\Drives\Microbee\cpmtools-2.10\tools');
  WorkingFolder := IncludeSlash(AInifile.ReadString('Locations', 'Working',
    ExtractFileDir(RUNCPM_exe)));

  If DirectoryExists(MOD_CPMTOOLS_bin) Then
    SetcpmtoolsPath(MOD_CPMTOOLS_bin)
  Else If DirectoryExists(CPMTOOLS_bin) Then
    SetcpmtoolsPath(CPMTOOLS_bin);

  If FileExists(UBEE512_exe) Then
    SetuBee512Path(UBEE512_exe);
End;

Procedure TSettings.SaveSettings(AInifile: TInifile);
Begin
  AInifile.WriteString('Locations', 'uBee512', UBEE512_exe);
  AInifile.WriteString('Locations', 'RunCPM', RUNCPM_exe);
  AInifile.WriteString('Locations', 'cpmtools', CPMTOOLS_bin);
  AInifile.WriteString('Locations', 'Modified cpmtools', MOD_CPMTOOLS_bin);
  AInifile.WriteString('Locations', 'Working', WorkingFolder);
End;

End.
