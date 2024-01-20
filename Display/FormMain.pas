Unit FormMain;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  EditBtn, StdCtrls, ShellCtrls, ExtCtrls, Buttons, Menus,
  FormSettings, Logging;

Type

  { TfrmMain }

  TfrmMain = Class(TForm)
    btnClearA: TSpeedButton;
    btnClearB: TSpeedButton;
    btnClearC: TSpeedButton;
    btnMacroExplorer: TBitBtn;
    cboFormatB: TComboBox;
    cboFormatC: TComboBox;
    cboModel: TComboBox;
    cboParallelPort: TComboBox;
    cboFormatA: TComboBox;
    cboTitle: TComboBox;
    cboType: TComboBox;
    edtCommandLine: TEdit;
    edtDiskA: TFileNameEdit;
    edtDiskB: TFileNameEdit;
    edtDiskC: TFileNameEdit;
    ilMain: TImageList;
    Label10: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblDiskA: TLabel;
    lblDiskB: TLabel;
    lblDiskC: TLabel;
    lblPP: TLabel;
    MainMenu1: TMainMenu;
    memRC: TMemo;
    mnuDebug: TMenuItem;
    pnlLeft: TPanel;
    pcOptions: TPageControl;
    Separator1: TMenuItem;
    mnuSettings: TMenuItem;
    mnuExit: TMenuItem;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    mnuFile: TMenuItem;
    pnluBee512: TPanel;
    Splitter1: TSplitter;
    ToolBar2: TToolBar;
    btnSettings: TToolButton;
    btnDiskExplorer: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    btnLaunchuBee512: TToolButton;
    ToolButton4: TToolButton;
    btnDebug: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    btnTest: TToolButton;
    ToolButton7: TToolButton;
    tsDrive: TTabSheet;
    tsROMs: TTabSheet;
    TabSheet3: TTabSheet;
    Procedure btnClearAClick(Sender: TObject);
    Procedure btnClearBClick(Sender: TObject);
    Procedure btnClearCClick(Sender: TObject);
    Procedure btnDiskExplorerClick(Sender: TObject);
    Procedure btnMacroExplorerClick(Sender: TObject);
    Procedure btnLaunchuBee512Click(Sender: TObject);
    Procedure btnTestClick(Sender: TObject);
    Procedure cboModelChange(Sender: TObject);
    Procedure cboTitleChange(Sender: TObject);
    Procedure cboTypeChange(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure mnuAboutClick(Sender: TObject);
    Procedure mnuExitClick(Sender: TObject);
    Procedure mnuDebugClick(Sender: TObject);
    Procedure mnuSettingsClick(Sender: TObject);
  Private
    FSettings: TSettings;
    FActivated: Boolean;
    FLoadingDSK: Boolean;
    FWorkingDir: String;
    FUpdatingCombos: Boolean;

    FLog: TLog;

    Function DriveFormatAsParam(AEdit: TFilenameEdit; AFormat: TCombobox): String;
    Procedure LoadRC;
    Procedure LoadSettings;
    Procedure RefreshRC;
    Procedure RefreshUI;
    Procedure SaveSettings;

    Procedure SetMacroCombo(ACombo: TComboBox; AValue: String);
    Procedure SetSelectedDisk(AFilename: String; AFilenameEdit: TFileNameEdit;
      AFormatCombo: TComboBox);
    Procedure SetSelectedFolder(AFolder: String; AFilenameEdit: TFileNameEdit;
      AFormatCombo: TComboBox);
  End;

Const
  DSK_ICO = 9;
  TXT_ICO = 10;

  DEFAULT_MODEL = 'p128k';
  DEFAULT_TITLE = 'Premium 128K';

Var
  frmMain: TfrmMain;

Implementation

Uses
  IniFiles, cpmtoolsSupport, CPMSupport, LazFileUtils, StringSupport,
  OSSupport, uBee512Support, FormMacroExplorer, FormDiskExplorer, FormDebug,
  FormAbout;

  {$R *.lfm}

{ TfrmMain }

Procedure TfrmMain.FormCreate(Sender: TObject);
Begin
  FUpdatingCombos := False;

  FActivated := False;
  FLoadingDSK := False;
  FSettings := TSettings.Create;

  FLog := TLog.Create(ChangeFileExt(Application.Exename, '.log'));
  Debug(LineEnding + '-----------------------');

  Debug(Application.ExeName);
End;

Procedure TfrmMain.FormActivate(Sender: TObject);
Begin
  If Not FActivated Then
  Begin
    LoadSettings;

    FActivated := True;
  End;
End;

Procedure TfrmMain.FormDestroy(Sender: TObject);
Begin
  SaveSettings;

  FreeAndNil(FSettings);
  FreeAndNil(FLog);
End;

Procedure TfrmMain.mnuAboutClick(Sender: TObject);
Begin
  FormAbout.ShowAbout;
End;

Procedure TfrmMain.mnuExitClick(Sender: TObject);
Begin
  Close;
End;

Procedure TfrmMain.mnuDebugClick(Sender: TObject);
Var
  oDebug: TfrmDebug;
Begin
  oDebug := TfrmDebug.Create(Self);
  Try
    oDebug.Load(FLog.Filename);

    oDebug.ShowModal;
  Finally
    oDebug.Free;
  End;
End;

Procedure TfrmMain.mnuSettingsClick(Sender: TObject);
Var
  oSettings: TfrmSettings;
Begin
  FSettings.WorkingFolder := FWorkingDir;

  oSettings := TfrmSettings.Create(Self);
  Try
    oSettings.Settings := FSettings;

    If oSettings.ShowModal = mrOk Then
    Begin
      Debug('Validating new settings');
      FSettings := oSettings.Settings;
      FSettings.ValidatePaths;

      LoadRC;

      FWorkingDir := FSettings.WorkingFolder;
    End;
  Finally
    oSettings.Free;
  End;
End;

Procedure TfrmMain.SetSelectedDisk(AFilename: String; AFilenameEdit: TFileNameEdit;
  AFormatCombo: TComboBox);
Var
  sFormat: String;
  iFormat: Integer;
Begin
  AFilename := Trim(AFilename);
  AFilenameEdit.Text := AFilename;

  If AFilename <> '' Then
  Begin
    // Make an educated guess as to Disk Format
    sFormat := DSKFormat(AFilename);
    iFormat := AFormatCombo.Items.IndexOf(sFormat);

    If iFormat >= 0 Then
      AFormatCombo.ItemIndex := iFormat
    Else
      AFormatCombo.Text := sFormat;
  End
  Else
    AFormatCombo.Text := 'Format?';

  RefreshUI;
End;

Procedure TfrmMain.SetSelectedFolder(AFolder: String; AFilenameEdit: TFileNameEdit;
  AFormatCombo: TComboBox);
Var
  sFormat: String;
  iFormat: Integer;
Begin
  AFolder := Trim(AFolder);
  AFilenameEdit.Text := AFolder;

  sFormat := 'rcpmfs/ds80';
  iFormat := AFormatCombo.Items.IndexOf(sFormat);

  If iFormat >= 0 Then
    AFormatCombo.ItemIndex := iFormat
  Else
    AFormatCombo.Text := sFormat;

  RefreshUI;
End;

Procedure TfrmMain.SetMacroCombo(ACombo: TComboBox; AValue: String);
Var
  iIndex: Integer;
Begin
  iIndex := ACombo.Items.IndexOf(AValue);
  If (iIndex >= 0) Then
  Begin
    ACombo.ItemIndex := iIndex;

    FUpdatingCombos := True;
    Try
      // Working around a Cocoa issue (.OnChange not firing when csDropdownlist
      If ACombo = cboType Then
        cboTypeChange(Self)
      Else If ACombo = cboModel Then
        cboModelChange(Self)
      Else If ACombo = cboTitle Then
        cboTitleChange(Self)
      Else
        ACombo.OnChange(Self);
    Finally
      FUpdatingCombos := False;
      RefreshRC;
    End;
  End;
End;

Procedure TfrmMain.LoadSettings;
Var
  iLeft, iWidth, iTop, iHeight: Integer;
  oIniFile: TIniFile;
  sModel, sTitle: String;
  sIniFile, sType: String;
  mtType: TMbeeType;

Begin
  sIniFile := ChangeFileExt(Application.Exename, '.ini');
  Debug('Loading ' + sInifile);
  FLog.IncIndent;

  oIniFile := TIniFile.Create(sIniFile);
  Try
    // Default, load Window Settings
    iLeft := oInifile.ReadInteger(Name, 'Left', Application.MainForm.Left);
    iTop := oInifile.ReadInteger(Name, 'Top', Application.MainForm.Top);
    iWidth := oInifile.ReadInteger(Name, 'Width', Application.MainForm.Width);
    iHeight := oInifile.ReadInteger(Name, 'Height', Application.MainForm.Height);

    SetBounds(iLeft, iTop, iWidth, iHeight);
    MakeFullyVisible;

    If oInifile.ReadBool(Name, 'Maximised', False) Then
      Application.MainForm.WindowState := wsMaximized
    Else
      Application.MainForm.WindowState := wsNormal;

    FSettings.LoadSettings(oIniFile);

    FWorkingDir := FSettings.WorkingFolder;

    LoadRC;

    SetSelectedDisk(FSettings.A, edtDiskA, cboFormatA);
    SetSelectedDisk(FSettings.B, edtDiskB, cboFormatB);
    SetSelectedDisk(FSettings.C, edtDiskC, cboFormatC);

    sModel := oIniFile.ReadString('Selected', 'Model', DEFAULT_MODEL);
    sTitle := oIniFile.ReadString('Selected', 'Title', DEFAULT_TITLE);

    mtType := uBee512.MbeeType(sModel);
    sType := MBTypeStr[mtType];

    SetMacroCombo(cboType, sType);
    SetMacroCombo(cboModel, sModel);
    SetMacroCombo(cboTitle, sTitle);
  Finally
    FLog.DecIndent;
    oInifile.Free;
  End;
End;

Procedure TfrmMain.SaveSettings;
Var
  oInifile: TIniFile;
  sInifile: String;
Begin
  sInifile := ChangeFileExt(Application.Exename, '.ini');
  Debug('Saving ' + sInifile);


  oInifile := TIniFile.Create(sInifile);

  // Do all the SaveSettings work in memory
  oInifile.CacheUpdates := True;
  Try
    oInifile.WriteBool(Name, 'Maximised', Application.MainForm.WindowState = wsMaximized);

    If WindowState <> wsMaximized Then
    Begin
      oInifile.WriteInteger(Name, 'Left', Application.MainForm.Left);
      oInifile.WriteInteger(Name, 'Top', Application.MainForm.Top);
      oInifile.WriteInteger(Name, 'Width', Application.MainForm.Width);
      oInifile.WriteInteger(Name, 'Height', Application.MainForm.Height);
    End;

    FSettings.WorkingFolder := FWorkingDir;
    FSettings.SaveSettings(oInifile);

    // No need to save Type, it's inferred from Model
    oIniFile.WriteString('Selected', 'Model', cboModel.Text);
    oIniFile.WriteString('Selected', 'Title', cboTitle.Text);

    // And flush the settings out in one go
    // This works around an AVG issue whereby
    // it locks the ini file during repeated writes
    // causing a CreateError exception to be thrown
    oInifile.UpdateFile;
  Finally
    oInifile.Free;
  End;
End;

Procedure TfrmMain.LoadRC;
Var
  iPrev: Integer;
Begin
  Debug(Format('Loading ubee512rc [%s]', [uBee512.RC]));
  FLog.IncIndent;
  Try
    uBee512.LoadRC;

    cboModel.Items.CommaText := ',' + uBee512.Models;

    If (cboModel.ItemIndex <> 0) And (cboModel.Items.Count > 0) Then
    Begin
      // Try and load the previous value instead of resetting
      iPrev := cboModel.Items.IndexOf(DEFAULT_MODEL);
      If iPrev >= 0 Then
        cboModel.ItemIndex := iPrev
      Else
        cboModel.ItemIndex := 0;
      cboModel.OnChange(Self);
    End;
  Finally
    FLog.DecIndent;
  End;
End;

Procedure TfrmMain.btnClearAClick(Sender: TObject);
Begin
  edtDiskA.Text := '';
  cboFormatA.Text := 'Format?';
  RefreshUI;
End;

Procedure TfrmMain.btnClearBClick(Sender: TObject);
Begin
  edtDiskB.Text := '';
  cboFormatB.Text := 'Format?';
  RefreshUI;
End;

Procedure TfrmMain.btnClearCClick(Sender: TObject);
Begin
  edtDiskC.Text := '';
  cboFormatC.Text := 'Format?';
  RefreshUI;
End;

Procedure TfrmMain.btnDiskExplorerClick(Sender: TObject);
Var
  oForm: TfrmDiskExplorer;
Begin
  oForm := TfrmDiskExplorer.Create(Self);

  FSettings.A := edtDiskA.Text;
  FSettings.B := edtDiskB.Text;
  FSettings.C := edtDiskC.Text;

  oForm.Settings := FSettings;
  Try
    If oForm.ShowModal = mrOk Then
    Begin
      FSettings.Assign(oForm.Settings);

      SetSelectedDisk(FSettings.A, edtDiskA, cboFormatA);
      SetSelectedDisk(FSettings.B, edtDiskB, cboFormatB);
      SetSelectedDisk(FSettings.C, edtDiskC, cboFormatC);
    End;
  Finally
    oForm.Free;
  End;
End;

Procedure TfrmMain.btnMacroExplorerClick(Sender: TObject);
Var
  oForm: TfrmDefinitionExplorer;
Begin
  oForm := TfrmDefinitionExplorer.Create(Self);
  Try
    oForm.Title := cboTitle.Text;
    If oForm.ShowModal = mrOk Then
    Begin
      SetMacroCombo(cboModel, oForm.Model);
      SetMacroCombo(cboTitle, oForm.Title);
    End;
  Finally
    oForm.Free;
  End;
End;

Procedure TfrmMain.cboTypeChange(Sender: TObject);
Var
  mtType: TMbeeType;
  iPrev: Integer;
Begin
  tsDrive.TabVisible := cboType.Text <> 'ROM';
  If tsDrive.TabVisible Then
    pcOptions.ActivePage := tsDrive;

  If cboType.ItemIndex >= 0 Then
  Begin
    mtType := TMbeeType(cboType.ItemIndex);

    cboModel.Items.CommaText := ',' + uBee512.ModelsByType(mtType);
    If (cboModel.ItemIndex <> 0) And (cboModel.Items.Count > 0) Then
    Begin
      iPrev := cboModel.Items.IndexOf(DEFAULT_MODEL);
      If iPrev >= 0 Then
        cboModel.ItemIndex := iPrev
      Else
        cboModel.ItemIndex := 0;

      cboModelChange(Self);
    End;
  End;

  RefreshRC;
End;

Procedure TfrmMain.RefreshRC;
Var
  sRC: String;
  oModel: TModel;
  oMacro: TSystemMacro;
  sParam: String;

  Procedure AddDisk(ADrive: String; AEdit: TFileNameEdit; ACombo: TComboBox);
  Var
    sFormat: String;
  Begin
    sFormat := DriveFormatAsParam(AEdit, ACombo);
    If sFormat <> '' Then
      sParam += Format(' %s -%s "%s"', [sFormat, ADrive, AEdit.Text]);
  End;

Begin
  If Not FUpdatingCombos Then
  Begin
    Debug('RefreshRC');

    sRC := 'Using defaults from ubee512rc.[global-start]';
    sParam := '';

    If (cboTitle.Text <> '') Then
    Begin
      oMacro := ubee512.MacroByTitle(cboTitle.Text);
      If assigned(oMacro) Then
      Begin
        sRC := '# ' + oMacro.Description + LineEnding;
        sRC += '[' + oMacro.Macro + ']' + LineEnding;
        sRC += oMacro.RC;

        sParam += ' ' + oMacro.Macro;
      End;
    End
    Else If (cboModel.Text <> '') Then
    Begin
      oModel := ubee512.Model(cboModel.Text);
      If Assigned(oModel) Then
      Begin
        sRC := '# ' + oModel.Description + LineEnding;
        sRC += '--model=' + oModel.Model;

        sParam += ' --model=' + oModel.Model;
      End;
    End;

    If tsDrive.TabVisible Then
    Begin
      AddDisk('a', edtDiskA, cboFormatA);
      AddDisk('b', edtDiskB, cboFormatB);
      AddDisk('c', edtDiskC, cboFormatC);
    End;

    edtCommandLine.Text := Trim(Format('"%s" %s', [uBee512.Exe, Trim(sParam)]));
    memRC.Lines.Text := sRC;
  End;
End;

Procedure TfrmMain.cboModelChange(Sender: TObject);
Var
  iPrev: Integer;
Begin
  cboTitle.Items.CommaText := ',' + uBee512.Titles(cboModel.Text);

  If (cboTitle.ItemIndex <> 0) And (cboTitle.Items.Count > 0) Then
  Begin
    iPrev := cboTitle.Items.IndexOf(DEFAULT_TITLE);
    If iPrev >= 0 Then
      cboTitle.ItemIndex := iPrev
    Else
      cboTitle.ItemIndex := 1;

    cboTitleChange(Self);
  End;

  RefreshUI;
End;

Procedure TfrmMain.cboTitleChange(Sender: TObject);
Var
  oMacro: TSystemMacro;
Begin
  oMacro := ubee512.MacroByTitle(cboTitle.Text);
  If Assigned(oMacro) Then
  Begin
    edtDiskA.Enabled := (Trim(oMacro.A) = '');
    edtDiskB.Enabled := (Trim(oMacro.B) = '');
    edtDiskC.Enabled := (Trim(oMacro.C) = '');

    cboFormatA.Enabled := edtDiskA.Enabled;
    cboFormatB.Enabled := edtDiskB.Enabled;
    cboFormatC.Enabled := edtDiskC.Enabled;

    btnClearA.Enabled := edtDiskA.Enabled;
    btnClearB.Enabled := edtDiskB.Enabled;
    btnClearC.Enabled := edtDiskC.Enabled;
  End;

  RefreshRC;
End;

Function TfrmMain.DriveFormatAsParam(AEdit: TFilenameEdit; AFormat: TCombobox): String;
Var
  sFormat: TCaption;
Begin
  Result := '';

  If Trim(AEdit.Text) = '' Then
    Exit;

  If FileExists(AEdit.Text) Then
  Begin
    sFormat := AFormat.Text;

    If (sFormat <> '') Then
      Result := '--format=' + sFormat;
  End
  Else If DirectoryExists(AEdit.Text) Then
    Result := '--type=rcpmfs --format=ds80';
End;

Procedure TfrmMain.btnLaunchuBee512Click(Sender: TObject);
Var
  sCommand, sResult, sDebug, s: String;
  bHasA: Boolean;
  slParams: TStringList;
  oMacro: TSystemMacro;

  Function AddDriveToCommand(ADrive: String; AEdit: TFilenameEdit; ACombo: TComboBox): Boolean;
  Var
    sFormat: String;
  Begin
    Result := Not AEdit.Enabled;

    If (Trim(AEdit.Text) = '') Then
      Exit;

    If AEdit.Enabled Then
    Begin
      sFormat := DriveFormatAsParam(AEdit, ACombo);
      If sFormat <> '' Then
      Begin
        If Pos('--type=rcpmfs', sFormat) > 0 Then
        Begin
          slParams.Add(TextBetween(sFormat, '', ' '));
          slParams.Add(TextBetween(sFormat, ' ', ''));
        End
        Else
          slParams.Add(sFormat);
        slParams.Add('-' + ADrive);
        slParams.Add(Format('%s', [AEdit.Text]));

        Result := True;
      End;
    End;
  End;

Begin
  slParams := TStringList.Create;
  Try
    sCommand := Format('%s', [FSettings.UBEE512_exe]);
    If cboTitle.Text <> '' Then
    Begin
      oMacro := ubee512.MacroByTitle(cboTitle.Text);
      slParams.Add(oMacro.Macro);
    End
    Else If cboModel.Text <> '' Then
      slParams.Add(Format('--model=%s', [cboModel.Text]));

    If tsDrive.TabVisible Then
    Begin
      bHasA := AddDriveToCommand('a', edtDiskA, cboFormatA);
      If bHasA Then
      Begin
        AddDriveToCommand('b', edtDiskB, cboFormatB);
        AddDriveToCommand('c', edtDiskC, cboFormatC);
      End;
    End;

    sDebug := '  ' + sCommand;
    For s In slParams Do
      If Pos(' ', s) > 0 Then
        sDebug += '  "' + s + '"'
      Else
        sDebug += '  ' + s;

    Debug('Launching ubee512 with:' + LineEnding + sDebug);
    edtCommandLine.Text := Trim(sDebug);
    edtCommandLine.Refresh;

    sResult := Trim(RunEx(sCommand, slParams));

    If sResult <> '' Then
      Debug('ubee512 returned:' + LineEnding + sResult);
  Finally
    slParams.Free;
  End;
End;

Procedure TfrmMain.btnTestClick(Sender: TObject);
Var
  sResult, sCommand: String;
Begin
  sCommand := Format('%s', [FSettings.UBEE512_exe]);
  sResult := Trim(RunEx(sCommand, ['--conio', '--echo', '@UBEE_USERHOME@', '--exit=-1']));
  Debug('Test ' + sResult);
End;

Procedure TfrmMain.RefreshUI;
Begin
  Debug('RefreshUI');
  btnLaunchuBee512.Enabled := (uBee512.Available) And (cboModel.Text <> '');

  RefreshRC;
End;

End.
