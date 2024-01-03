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
    lvSystemMacros: TListView;
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
    Procedure lvSystemMacrosSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  Private
    FActivated: Boolean;
    FSettings: TSettings;
    Procedure LoadRC;
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

Procedure TfrmSettings.lvSystemMacrosSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
Begin
  If (Selected) And (Assigned(Item)) Then
    memSystems.Lines.Text := RC(Item.Caption)
  Else
    memSystems.Lines.Clear;
End;

Procedure TfrmSettings.FormActivate(Sender: TObject);
Begin
  If Not FActivated Then
  Begin
    edtuBee512.Text := FSettings.UBEE512_exe;
    edtRunCPM.Text := FSettings.RUNCPM_exe;
    edtCPMTools.Text := FSettings.CPMTOOLS_bin;
    edtCPMToolsModified.Text := FSettings.MOD_CPMTOOLS_bin;

    LoadRC;

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

Procedure TfrmSettings.LoadRC;
Var
  oMacro: TSystemMacro;
  oItem: TListItem;
  i: Integer;
Begin
  If uBee512Available And FileExists(uBee512RCPath) Then
  Begin
    SetBusy;
    Try
      memRC.Lines.LoadFromFile(uBee512RCPath);
      uBee512Support.LoadRC;

      lvSystemMacros.Items.Clear;
      memSystems.Lines.Clear;
      lvSystemMacros.Items.BeginUpdate;

      For i := 0 To SystemMacros.Count - 1 Do
      Begin
        oMacro := SystemMacros[i];

        If oMacro.Title <> '' Then
        Begin
          oItem := lvSystemMacros.Items.Add;

          oItem.Caption := oMacro.Macro;
          oItem.SubItems.Add(oMacro.Model);
          oItem.SubItems.Add(oMacro.Title);
          oItem.SubItems.Add(oMacro.A);
          //oItem.SubItems.Add(oMacro.Col);
          oItem.SubItems.Add(oMacro.SRAM + ' ' + oMacro.SRAM_File);
          //oItem.SubItems.Add(oMacro.Status);
          oItem.SubItems.Add(oMacro.Description);
        End;
      End;
    Finally
      lvSystemMacros.Items.EndUpdate;
      lvSystemMacros.AutoSize := True;
      ClearBusy;
    End;
  End;
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
