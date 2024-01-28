Unit FormMain;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  EditBtn, StdCtrls, ShellCtrls, ExtCtrls, Buttons, Menus,
  FormSettings, Logs;

Type

  { TfrmMain }

  TfrmMain = Class(TForm)
    btnClearA: TSpeedButton;
    btnClearB: TSpeedButton;
    btnClearC: TSpeedButton;
    btnDefinitionExplorer: TBitBtn;
    cboFormatB: TComboBox;
    cboFormatC: TComboBox;
    cboModel: TComboBox;
    cboParallelPort: TComboBox;
    cboFormatA: TComboBox;
    cboTitle: TComboBox;
    cboType: TComboBox;
    lblDiskAlias: TLabel;
    lblROMAlias: TLabel;
    memCommandLine: TMemo;
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
    memDiskAlias: TMemo;
    memROMAlias: TMemo;
    memRC: TMemo;
    Separator2: TMenuItem;
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
    Procedure btnDefinitionExplorerClick(Sender: TObject);
    Procedure btnLaunchuBee512Click(Sender: TObject);
    Procedure btnTestClick(Sender: TObject);
    Procedure DiskorFormatChange(Sender: TObject);
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
    FUpdatingCombos: Boolean;

    FLog: TLog;

    Function DriveFormatAsParam(AEdit: TFilenameEdit; AFormat: TCombobox): String;
    Procedure LoadRC;
    Procedure LoadSettings;
    Procedure RefreshRC;
    Procedure RefreshUI;
    Procedure SaveSettings;

    Procedure SetDefinitionCombo(ACombo: TComboBox; AValue: String);
    Procedure SetSelectedDisk(AFilename: String; AFormat: String;
      AFilenameEdit: TFileNameEdit; AFormatCombo: TComboBox);
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
  IniFiles, cpmtoolsSupport, LazFileUtils, StringSupport, FileSupport,
  OSSupport, uBee512Support, FormDefinitionExplorer, FormDiskExplorer, FormDebug,
  FormAbout;

  {$R *.lfm}

{ TfrmMain }

Procedure TfrmMain.FormCreate(Sender: TObject);
Begin
  {$IFDEF DARWIN}
  cboType.Style := csDropDown;
  cboModel.Style := csDropDown;
  cboTitle.Style := csDropDown;
  cboFormatA.Style := csDropDown;
  cboFormatB.Style := csDropDown;
  cboFormatC.Style := csDropDown;
  {$ENDIF}

  FUpdatingCombos := False;

  FActivated := False;
  FLoadingDSK := False;
  FSettings := TSettings.Create;

  FLog := TLog.Create(IncludeSlash(FSettings.Folder) + 'debug.log');
  Debug(LineEnding + '-----------------------');
  Debug(Application.ExeName);
  Debug(FLog.Filename);
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
  oForm: TfrmSettings;
Begin
  oForm := TfrmSettings.Create(Self);
  Try
    oForm.Settings := FSettings;

    If oForm.ShowModal = mrOk Then
    Begin
      Debug('Validating new settings');
      FSettings.Assign(oForm.Settings);
      FSettings.ValidatePaths;

      LoadRC;
    End;
  Finally
    oForm.Free;
  End;
End;

Procedure TfrmMain.SetSelectedDisk(AFilename: String; AFormat: String;
  AFilenameEdit: TFileNameEdit; AFormatCombo: TComboBox);
Begin
  AFilename := Trim(AFilename);
  AFilenameEdit.Text := AFilename;
  AFormatCombo.Text := AFormat;

  RefreshUI;
End;

Procedure TfrmMain.SetDefinitionCombo(ACombo: TComboBox; AValue: String);
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
  sIniFile := IncludeSlash(FSettings.Folder) + 'settings.ini';
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

    LoadRC;

    SetSelectedDisk(FSettings.A, FSettings.A_Format, edtDiskA, cboFormatA);
    SetSelectedDisk(FSettings.B, FSettings.B_Format, edtDiskB, cboFormatB);
    SetSelectedDisk(FSettings.C, FSettings.C_Format, edtDiskC, cboFormatC);

    sModel := oIniFile.ReadString('Selected', 'Model', DEFAULT_MODEL);
    sTitle := oIniFile.ReadString('Selected', 'Title', DEFAULT_TITLE);

    mtType := uBee512.Models.MbeeType(sModel);
    sType := MBTypeStr[mtType];

    SetDefinitionCombo(cboType, sType);
    SetDefinitionCombo(cboModel, sModel);
    SetDefinitionCombo(cboTitle, sTitle);
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
  sIniFile := IncludeSlash(FSettings.Folder) + 'settings.ini';
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

    FSettings.A := edtDiskA.Text;
    FSettings.B := edtDiskB.Text;
    FSettings.C := edtDiskC.Text;

    FSettings.A_Format := cboFormatA.Text;
    FSettings.B_Format := cboFormatB.Text;
    FSettings.C_Format := cboFormatC.Text;

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
  bNew: Boolean;
  oAlias: TDiskAlias;
  sAliasFile, sPartialAlias: String;
  slTemp: TStringList;

Begin
  Debug(Format('Loading ubee512rc [%s]', [uBee512.RC]));
  FLog.IncIndent;
  Try
    bNew := uBee512.LoadRC;

    If bNew Then
    Begin
      If FileExists(uBee512.DiskAliases.Filename) Then
      Begin
        // Analyse "disks.alias"
        slTemp := TStringList.Create;
        Try
          sPartialAlias := '';
          For oAlias In uBee512.DiskAliases Do
          Begin
            If (Trim(oAlias.Alias) <> '') And (Trim(oAlias.Filename) <> '') Then
              slTemp.Add('  %s %s', [oAlias.Alias, oAlias.Filename]);

            If (Trim(oAlias.Alias) <> '') And (Trim(oAlias.Filename) = '') Then
              sPartialAlias += oAlias.Alias + ', ';
          End;
          sPartialAlias := TrimChars(sPartialAlias, [' ', ',']);

          memDiskAlias.Lines.Clear;
          memDiskAlias.Lines.Add('There are %d aliases in "%s"',
            [uBee512.DiskAliases.Count, uBee512.DiskAliases.Filename]);

          memDiskAlias.Lines.Add('');
          If slTemp.Count > 0 Then
          Begin
            memDiskAlias.Lines.Add('The following %d entries are fully defined:', [slTemp.Count]);
            memDiskAlias.Lines.AddStrings(slTemp);
          End
          Else
            memDiskAlias.Lines.Add('There are no defined aliases');

          If (sPartialAlias <> '') Then
          Begin
            memDiskAlias.Lines.Add('');
            memDiskAlias.Lines.Add('The following entries are partially defined:');
            memDiskAlias.Lines.Add('  ' + sPartialAlias);
          End;
        Finally
          slTemp.Free;
        End;
      End
      Else
        memDiskAlias.Lines.Add('File %s not found', [uBee512.DiskAliases.Filename]);

      // TODO Implement roms.alias summary correctly
      sAliasFile := IncludeSlash(ubee512.WorkingDir) + 'roms.alias';
      If FileExists(sAliasFile) Then
        memROMAlias.Lines.LoadFromFile(sAliasFile);
    End;

    cboModel.Items.CommaText := ',' + uBee512.Definitions.Models;

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
  cboFormatA.Text := '';
  RefreshUI;
End;

Procedure TfrmMain.btnClearBClick(Sender: TObject);
Begin
  edtDiskB.Text := '';
  cboFormatB.Text := '';
  RefreshUI;
End;

Procedure TfrmMain.btnClearCClick(Sender: TObject);
Begin
  edtDiskC.Text := '';
  cboFormatC.Text := '';
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

      SetSelectedDisk(FSettings.A, FSettings.A_Format, edtDiskA, cboFormatA);
      SetSelectedDisk(FSettings.B, FSettings.B_Format, edtDiskB, cboFormatB);
      SetSelectedDisk(FSettings.C, FSettings.C_Format, edtDiskC, cboFormatC);
    End;
  Finally
    oForm.Free;
  End;
End;

Procedure TfrmMain.btnDefinitionExplorerClick(Sender: TObject);
Var
  oForm: TfrmDefinitionExplorer;
Begin
  oForm := TfrmDefinitionExplorer.Create(Self);
  Try
    oForm.Title := cboTitle.Text;
    If oForm.ShowModal = mrOk Then
    Begin
      SetDefinitionCombo(cboModel, oForm.Model);
      SetDefinitionCombo(cboTitle, oForm.Title);
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

    cboModel.Items.CommaText := ',' + uBee512.Definitions.ModelsByType(mtType);
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
  oDefinition: TDefinition;
  sParam: String;

  Procedure AddDisk(ADrive: String; AEdit: TFileNameEdit; ACombo: TComboBox);
  Var
    sFormat: String;
  Begin
    sFormat := DriveFormatAsParam(AEdit, ACombo);

    If (AEdit.Enabled) And (Trim(AEdit.Text) <> '') Then
      sParam += ' ' + Trim(Format('%s -%s "%s"', [sFormat, ADrive, AEdit.Text]));
  End;

Begin
  If Not FUpdatingCombos Then
  Begin
    Debug('RefreshRC');

    sRC := 'Using defaults from ubee512rc.[global-start]';
    sParam := '';

    If (cboTitle.Text <> '') Then
    Begin
      oModel := ubee512.Models[cboModel.Text];
      oDefinition := ubee512.Definitions.DefinitionByTitle(cboTitle.Text);
      If assigned(oDefinition) Then
      Begin
        If Assigned(oModel) Then
          sRC := '# ' + oModel.Description + LineEnding
        Else
          sRC := '# ' + oDefinition.Description + LineEnding;
        sRC += '[' + oDefinition.Definition + ']' + LineEnding;
        sRC += oDefinition.RC;

        sParam += ' ' + oDefinition.Definition;
      End;
    End
    Else If (cboModel.Text <> '') Then
    Begin
      oModel := ubee512.Models[cboModel.Text];
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

    memCommandLine.Lines.Text := Trim(Format('>"%s" %s', [uBee512.Exe, Trim(sParam)]));
    memRC.Lines.Text := sRC;
  End;
End;

Procedure TfrmMain.cboModelChange(Sender: TObject);
Var
  iPrev: Integer;
Begin
  cboTitle.Items.CommaText := ',' + uBee512.Definitions.Titles(cboModel.Text);

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
  oDefinition: TDefinition;
Begin
  oDefinition := ubee512.Definitions.DefinitionByTitle(cboTitle.Text);
  If Assigned(oDefinition) Then
  Begin
    edtDiskA.Enabled := (Trim(oDefinition.A) = '');
    edtDiskB.Enabled := (Trim(oDefinition.B) = '');
    edtDiskC.Enabled := (Trim(oDefinition.C) = '');

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
    If AFormat.Text = '' Then
      Result := '--type=rcpmfs --format=ds80'
    Else
      Result := '--type=rcpmfs --format=' + Trim(AFormat.Text);
End;

Procedure TfrmMain.btnLaunchuBee512Click(Sender: TObject);
Var
  sCommand, sResult, sDebug, s: String;
  bHasA: Boolean;
  slParams: TStringList;
  oDefinition: TDefinition;

  Function AddDriveToCommand(ADrive: String; AEdit: TFilenameEdit; ACombo: TComboBox): Boolean;
  Var
    sFormat: String;
  Begin
    Result := Not AEdit.Enabled;

    If (Trim(AEdit.Text) = '') Then
      Exit;

    If (AEdit.Enabled) Then
    Begin
      sFormat := DriveFormatAsParam(AEdit, ACombo);
      If sFormat <> '' Then
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

Begin
  slParams := TStringList.Create;
  Try
    sCommand := Format('%s', [FSettings.UBEE512_exe]);
    If cboTitle.Text <> '' Then
    Begin
      oDefinition := ubee512.Definitions.DefinitionByTitle(cboTitle.Text);
      slParams.Add(oDefinition.Definition);
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
    memCommandLine.Lines.Text := '>' + Trim(sDebug);
    memCommandLine.Refresh;

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

Procedure TfrmMain.DiskorFormatChange(Sender: TObject);
Begin
  RefreshRC;
End;

Procedure TfrmMain.RefreshUI;
Begin
  Debug('RefreshUI');
  btnLaunchuBee512.Enabled := (uBee512.Available) And (cboModel.Text <> '');

  RefreshRC;
End;

End.
