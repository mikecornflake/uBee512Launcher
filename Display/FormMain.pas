Unit FormMain;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ShellCtrls, ExtCtrls, Buttons, Menus, IniPropStorage, IpHtml,
  DialogSettings, Logs;

Type

  { TfrmMain }

  TfrmMain = Class(TForm)
    btnClearA: TSpeedButton;
    btnExploreA: TSpeedButton;
    btnClearB: TSpeedButton;
    btnClearC: TSpeedButton;
    btnDefinitionExplorer: TBitBtn;
    btnExploreB: TSpeedButton;
    btnExploreA2: TSpeedButton;
    btnExploreC: TSpeedButton;
    cboFormatB: TComboBox;
    cboFormatC: TComboBox;
    cboModel: TComboBox;
    cboParallelPort: TComboBox;
    cboFormatA: TComboBox;
    cboTitle: TComboBox;
    cboType: TComboBox;
    cboDiskA: TComboBox;
    cboDiskB: TComboBox;
    cboDiskC: TComboBox;
    iniPropStorage: TIniPropStorage;
    lblDiskAlias: TLabel;
    lblROMAlias: TLabel;
    memCommandLine: TMemo;
    ilMain: TImageList;
    Label10: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblDiskA: TLabel;
    lblDiskB: TLabel;
    lblDiskC: TLabel;
    lblPP: TLabel;
    MainMenu1: TMainMenu;
    htmlRC: TIpHtmlPanel;
    memROMAlias: TMemo;
    htmlDiskAlias: TIpHtmlPanel;
    pnlRCSummary: TPanel;
    htmlSummary: TIpHtmlPanel;
    rgVideoType: TRadioGroup;
    rgMonitor: TRadioGroup;
    rgColour: TRadioGroup;
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
    Splitter2: TSplitter;
    tsVideo: TTabSheet;
    ToolBar2: TToolBar;
    btnSettings: TToolButton;
    btnDiskExplorer: TToolButton;
    btnDiskAlias: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    btnLaunchuBee512: TToolButton;
    ToolButton4: TToolButton;
    btnDebug: TToolButton;
    ToolButton5: TToolButton;
    btnTest: TToolButton;
    ToolButton7: TToolButton;
    tsDrive: TTabSheet;
    tsROMs: TTabSheet;
    TabSheet3: TTabSheet;
    Procedure btnClearAClick(Sender: TObject);
    Procedure btnClearBClick(Sender: TObject);
    Procedure btnClearCClick(Sender: TObject);
    Procedure btnDiskAliasClick(Sender: TObject);
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
    Procedure rgVideoTypeSelectionChanged(Sender: TObject);
  Private
    FSettings: TSettings;
    FActivated: Boolean;
    FLoadingDSK: Boolean;
    FUpdatingCombos: Integer;

    FLog: TLog;

    Function DriveFormatAsParam(AEdit: TComboBox; AFormat: TCombobox): String;
    Procedure LoadRC;
    Procedure LoadSettings;
    Procedure RefreshDiskAliasSummary;
    Procedure RefreshRC;
    Procedure RefreshUI;
    Procedure SaveSettings;

    Function VideoTypeOpt: String;
    Function MonitorOpt: String;
    Function ColourOpt: String;

    Procedure SetDefinitionCombo(ACombo: TComboBox; AValue: String);
    Procedure SetSelectedDisk(AFilename: String; AFormat: String;
      AFilenameEdit: TComboBox; AFormatCombo: TComboBox);
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
  IniFiles, LazFileUtils, Clipbrd,
  Validators, cpmtoolsSupport, StringSupport, FileSupport, ControlsSupport,
  OSSupport, uBee512Support, VersionSupport,
  DialogDefinitionExplorer, DialogDiskExplorer, DialogDebug, DialogAbout,
  DialogDiskAlias;

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

  FUpdatingCombos := 0;

  FActivated := False;
  FLoadingDSK := False;
  FSettings := TSettings.Create;
  FSettings.Filename := IncludeSlash(FSettings.Folder) + 'settings.ini';

  iniPropStorage.IniFileName := FSettings.Filename;
  iniPropStorage.IniSection := Self.Name;

  FLog := TLog.Create(IncludeSlash(FSettings.Folder) + 'debug.log');
  Debug(LineEnding + '-----------------------');
  Debug(Application.ExeName);
  Debug(FLog.Filename);

  Caption := Format('%s %s', [Application.Title, GetFileVersion]);
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
  DialogAbout.ShowAbout;
End;

Procedure TfrmMain.mnuExitClick(Sender: TObject);
Begin
  Close;
End;

Procedure TfrmMain.mnuDebugClick(Sender: TObject);
Var
  oDebug: TdlgDebug;
Begin
  oDebug := TdlgDebug.Create(Self);
  Try
    oDebug.Load(FLog.Filename);

    oDebug.ShowModal;
  Finally
    oDebug.Free;
  End;
End;

Procedure TfrmMain.mnuSettingsClick(Sender: TObject);
Var
  oForm: TdlgSettings;
Begin
  oForm := TdlgSettings.Create(Self);
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

Procedure TfrmMain.rgVideoTypeSelectionChanged(Sender: TObject);
Begin
  RefreshRC;
End;

Procedure TfrmMain.RefreshUI;
Begin
  Debug('RefreshUI (FUpdatingCombos=%d)', [FUpdatingCombos]);
  btnLaunchuBee512.Enabled := (uBee512.Available) And (cboModel.Text <> '');

  RefreshRC;
End;

Procedure TfrmMain.RefreshRC;
Var
  sRC: String;
  oModel: TModel;
  oDefinition: TDefinition;
  sParam, sSummary: String;
  elMaxErrorLevel, elTemp: TErrorLevel;
  arrSummary: TStringArray;

  Procedure AddDisk(ADrive: String; AEdit: TComboBox; ACombo: TComboBox);
  Var
    sFormat: String;
  Begin
    sFormat := DriveFormatAsParam(AEdit, ACombo);

    If (AEdit.Enabled) And (Trim(AEdit.Text) <> '') Then
      sParam += ' ' + Trim(Format('%s -%s "%s"', [sFormat, ADrive, AEdit.Text]));
  End;

Begin
  If FUpdatingCombos = 0 Then
  Begin
    Debug('RefreshRC');

    sRC := 'Using defaults from ubee512rc.[global-start]';
    sParam := '';

    oDefinition := nil;
    oModel := nil;

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
      AddDisk('a', cboDiskA, cboFormatA);
      AddDisk('b', cboDiskB, cboFormatB);
      AddDisk('c', cboDiskC, cboFormatC);
    End;

    sParam := Trim(VideoTypeOpt + ' ' + ColourOpt + ' ' + MonitorOpt) + sParam;

    memCommandLine.Lines.Text := Trim(Format('>"%s" %s', [uBee512.Exe, Trim(sParam)]));
    SetHTML(htmlRC, '<body>' + ValidateHTML(sRC) + '</body>');

    // Display the results of the validity checks relating to the current settings
    arrSummary := nil;
    SetLength(arrSummary, 0);

    AddStringsToArray(arrSummary, uBee512.Validator.Summary(True));
    elMaxErrorLevel := uBee512.Validator.ErrorLevel;

    If Assigned(oDefinition) Then
    Begin
      oDefinition.Validator.Process;

      If Length(arrSummary) > 0 Then
        AddStringToArray(arrSummary, '');

      AddStringsToArray(arrSummary, oDefinition.Validator.Summary(True));

      elTemp := oDefinition.Validator.ErrorLevel;
      If elTemp > elMaxErrorLevel Then
        elMaxErrorLevel := elTemp;
    End;

    If Assigned(oModel) Then
    Begin
      oModel.Validator.Process;

      If Length(arrSummary) > 0 Then
        AddStringToArray(arrSummary, '');

      AddStringsToArray(arrSummary, oModel.Validator.Summary(True));

      elTemp := oModel.Validator.ErrorLevel;
      If elTemp > elMaxErrorLevel Then
        elMaxErrorLevel := elTemp;
    End;

    sSummary := ArrayToString(arrSummary);

    SetHTML(htmlSummary, '<body>' + sSummary + '</body>');
  End;
End;

Procedure TfrmMain.SetSelectedDisk(AFilename: String; AFormat: String;
  AFilenameEdit: TComboBox; AFormatCombo: TComboBox);
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

    Inc(FUpdatingCombos);
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
      Dec(FUpdatingCombos);
      RefreshRC;
    End;
  End;
End;

Procedure TfrmMain.LoadSettings;
Var
  iLeft, iWidth, iTop, iHeight: Integer;
  oIniFile: TIniFile;
  sModel, sTitle: String;
  sType: String;
  mtType: TMbeeType;
Begin
  Debug('Loading ' + FSettings.Filename);
  FLog.IncIndent;

  oIniFile := TIniFile.Create(FSettings.Filename);
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

    Inc(FUpdatingCombos);
    Try
      LoadRC;

      SetSelectedDisk(FSettings.A, FSettings.A_Format, cboDiskA, cboFormatA);
      SetSelectedDisk(FSettings.B, FSettings.B_Format, cboDiskB, cboFormatB);
      SetSelectedDisk(FSettings.C, FSettings.C_Format, cboDiskC, cboFormatC);

      sModel := oIniFile.ReadString('Selected', 'Model', DEFAULT_MODEL);
      sTitle := oIniFile.ReadString('Selected', 'Title', DEFAULT_TITLE);

      mtType := uBee512.Models.MbeeType(sModel);
      sType := MBTypeStr[mtType];

      SetDefinitionCombo(cboType, sType);
      SetDefinitionCombo(cboModel, sModel);
      SetDefinitionCombo(cboTitle, sTitle);

      rgColour.ItemIndex := FSettings.ColourOpt;
      rgMonitor.ItemIndex := FSettings.MonitorOpt;
      rgVideoType.ItemIndex := FSettings.VideoTypeOpt;
    Finally
      Dec(FUpdatingCombos);
      RefreshRC;
    End;
  Finally
    FLog.DecIndent;
    oInifile.Free;
  End;
End;

Procedure TfrmMain.SaveSettings;
Var
  oInifile: TIniFile;
Begin
  Debug('Saving ' + FSettings.Filename);

  oIniFile := TIniFile.Create(FSettings.Filename);

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

    FSettings.A := cboDiskA.Text;
    FSettings.B := cboDiskB.Text;
    FSettings.C := cboDiskC.Text;

    FSettings.A_Format := cboFormatA.Text;
    FSettings.B_Format := cboFormatB.Text;
    FSettings.C_Format := cboFormatC.Text;

    FSettings.ColourOpt := rgColour.ItemIndex;
    FSettings.MonitorOpt := rgMonitor.ItemIndex;
    FSettings.VideoTypeOpt := rgVideoType.ItemIndex;

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

Function TfrmMain.VideoTypeOpt: String;
Begin
  Case rgVideoType.ItemIndex Of
    1: Result := '--video-type=gl';
    2: Result := '--video-type=hw';
    3: Result := '--video-type=sw';
    Else
      Result := ''
  End;
End;

Function TfrmMain.MonitorOpt: String;
Begin
  Case rgMonitor.ItemIndex Of
    1: Result := '--monitor=a';
    2: Result := '--monitor=g';
    3: Result := '--monitor=w';
    4: Result := '--monitor=b';
    5: Result := '--monitor=c';
    Else
      Result := ''
  End;
End;

Function TfrmMain.ColourOpt: String;
Begin
  Case rgColour.ItemIndex Of
    1: Result := '--mono';
    2: Result := '--col-type=0';
    3: Result := '--col-type=1';
    Else
      Result := '';
  End;
End;

Procedure TfrmMain.RefreshDiskAliasSummary;
Var
  sAlias: String;
  iAlias, iInfo, iError, iWarning: Int64;
Begin
  sAlias := '';
  If FileExists(uBee512.DiskAliases.Filename) Then
  Begin
    // Analyse "disks.alias"
    Debug('Refresh summary of ' + uBee512.DiskAliases.Filename);

    iAlias := uBee512.DiskAliases.Validators.Count([elInfo, elWarning, elError]);
    sAlias += Format('<p>There are %d aliases in "%s"</p>',
      [iAlias, uBee512.DiskAliases.Filename]);

    sAlias += '<br>' + LineEnding + '<p>';
    iInfo := uBee512.DiskAliases.Validators.Count([elInfo]);
    If iInfo > 0 Then
    Begin
      sAlias += Format('<b>The following %d entries are correct and ready to use:</b><br>',
        [iInfo]);
      sAlias += LineEnding;
      sAlias += ArrayToString(uBee512.DiskAliases.Validators.Summary([elInfo]));
    End
    Else
      sAlias += 'There are no defined aliases ready to use';
    sAlias += '</p>' + LineEnding;

    iError := uBee512.DiskAliases.Validators.Count([elError]);
    If iError > 0 Then
    Begin
      sAlias += '<br>' + LineEnding + '<p>';
      sAlias += Format('<b>The following %d errors were found:</b><br>', [iError]);
      sAlias += ArrayToString(uBee512.DiskAliases.Validators.Summary([elError]));
      sAlias += '</p>';
    End
    Else
      sAlias += 'No errors were found';

    sAlias += '<br>' + LineEnding + '<p>';
    iWarning := uBee512.DiskAliases.Validators.Count([elWarning]);
    If iWarning > 0 Then
    Begin
      sAlias += Format('<b>The following %d warnings were found:</b><br>', [iWarning]);
      sAlias += ArrayToString(uBee512.DiskAliases.Validators.Summary([elWarning]));
    End
    Else
      sAlias += 'No warnings were found';
    sAlias += '</p>';
  End
  Else
    sAlias += Format('File %s not found<br>', [uBee512.DiskAliases.Filename]);

  Clipboard.AsText := '<body>' + sAlias + '</body>';
  SetHTML(htmlDiskAlias, '<body>' + sAlias + '</body>');
End;

Procedure TfrmMain.LoadRC;
Var
  iPrev: Integer;
  bNew: Boolean;
  sAliasFile: String;
Begin
  Debug(Format('Loading ubee512rc [%s]', [uBee512.RC]));
  FLog.IncIndent;
  Try
    bNew := uBee512.LoadRC;

    If bNew Then
    Begin
      Caption := Format('%s %s:  uBee512 Folder=[%s]', [Application.Title,
        GetFileVersion, uBee512.WorkingDir]);

      RefreshDiskAliasSummary;

      // TODO Implement roms.alias summary correctly
      sAliasFile := IncludeSlash(ubee512.WorkingDir) + 'roms.alias';
      If FileExists(sAliasFile) Then
        memROMAlias.Lines.LoadFromFile(sAliasFile);

      cboDiskA.Items.Clear;
      cboDiskB.Items.Clear;
      cboDiskC.Items.Clear;

      cboDiskA.Items.AddStrings(uBee512.DiskAliases.ValidAliases);
      cboDiskB.Items.AddStrings(cboDiskA.Items);
      cboDiskC.Items.AddStrings(cboDiskA.Items);
    End;

    Inc(FUpdatingCombos);
    Try
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
      Dec(FUpdatingCombos);
    End;
  Finally
    FLog.DecIndent;
  End;
End;

Procedure TfrmMain.btnClearAClick(Sender: TObject);
Begin
  cboDiskA.Text := '';
  cboFormatA.Text := '';
  RefreshUI;
End;

Procedure TfrmMain.btnClearBClick(Sender: TObject);
Begin
  cboDiskB.Text := '';
  cboFormatB.Text := '';
  RefreshUI;
End;

Procedure TfrmMain.btnClearCClick(Sender: TObject);
Begin
  cboDiskC.Text := '';
  cboFormatC.Text := '';
  RefreshUI;
End;

Procedure TfrmMain.btnDiskAliasClick(Sender: TObject);
Var
  oForm: TdlgDiskAlias;
Begin
  oForm := TdlgDiskAlias.Create(Self);

  Try
    If oForm.ShowModal = mrOk Then
      uBee512.DiskAliases.Save  // commit
    Else
      uBee512.DiskAliases.Load; // undo

    RefreshDiskAliasSummary;
  Finally
  End;
End;

Procedure TfrmMain.btnDiskExplorerClick(Sender: TObject);
Var
  oForm: TdlgDiskExplorer;
Begin
  oForm := TdlgDiskExplorer.Create(Self);

  FSettings.A := cboDiskA.Text;
  FSettings.B := cboDiskB.Text;
  FSettings.C := cboDiskC.Text;

  oForm.Settings := FSettings;
  Try
    If oForm.ShowModal = mrOk Then
    Begin
      FSettings.Assign(oForm.Settings);

      Inc(FUpdatingCombos);
      Try
        SetSelectedDisk(FSettings.A, FSettings.A_Format, cboDiskA, cboFormatA);
        SetSelectedDisk(FSettings.B, FSettings.B_Format, cboDiskB, cboFormatB);
        SetSelectedDisk(FSettings.C, FSettings.C_Format, cboDiskC, cboFormatC);
      Finally
        Dec(FUpdatingCombos);
      End;

      uBee512.DiskAliases.Save;
    End
    Else // Undo any changes made
      uBee512.DiskAliases.Load;

    RefreshDiskAliasSummary;
  Finally
    oForm.Free;
  End;
End;

Procedure TfrmMain.btnDefinitionExplorerClick(Sender: TObject);
Var
  oForm: TdlgDefinitionExplorer;
Begin
  oForm := TdlgDefinitionExplorer.Create(Self);
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
  sPrev: String;
Begin
  sPrev := cboModel.Text;

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

      If (sPrev <> cboModel.Text) Then
        cboModelChange(Self);
    End;
  End;

  If (sPrev <> cboModel.Text) Then
    RefreshRC;
End;

Procedure TfrmMain.cboModelChange(Sender: TObject);
Var
  iPrev: Integer;
  sPrev: TCaption;
Begin
  sPrev := cboTitle.Text;

  cboTitle.Items.CommaText := ',' + uBee512.Definitions.Titles(cboModel.Text);

  If (cboTitle.ItemIndex <> 0) And (cboTitle.Items.Count > 0) Then
  Begin
    iPrev := cboTitle.Items.IndexOf(DEFAULT_TITLE);
    If iPrev >= 0 Then
      cboTitle.ItemIndex := iPrev
    Else
      cboTitle.ItemIndex := 1;

    If (sPrev <> cboTitle.Text) Then
      cboTitleChange(Self);
  End;

  If (sPrev <> cboTitle.Text) Then
    RefreshUI;
End;

Procedure TfrmMain.cboTitleChange(Sender: TObject);
Var
  oDefinition: TDefinition;
Begin
  oDefinition := ubee512.Definitions.DefinitionByTitle(cboTitle.Text);
  If Assigned(oDefinition) Then
  Begin
    cboDiskA.Enabled := (Trim(oDefinition.A) = '');
    cboDiskB.Enabled := (Trim(oDefinition.B) = '');
    cboDiskC.Enabled := (Trim(oDefinition.C) = '');

    cboFormatA.Enabled := cboDiskA.Enabled;
    cboFormatB.Enabled := cboDiskB.Enabled;
    cboFormatC.Enabled := cboDiskC.Enabled;

    btnClearA.Enabled := cboDiskA.Enabled;
    btnClearB.Enabled := cboDiskB.Enabled;
    btnClearC.Enabled := cboDiskC.Enabled;
  End;

  RefreshRC;
End;

Function TfrmMain.DriveFormatAsParam(AEdit: TComboBox; AFormat: TCombobox): String;
Var
  sFormat: String;
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

  Function AddDriveToCommand(ADrive: String; AEdit: TComboBox; ACombo: TComboBox): Boolean;
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

    If VideoTypeOpt <> '' Then
      slParams.Add(VideoTypeOpt);

    If ColourOpt <> '' Then
      slParams.Add(ColourOpt);

    If MonitorOpt <> '' Then
      slParams.Add(MonitorOpt);

    If cboTitle.Text <> '' Then
    Begin
      oDefinition := ubee512.Definitions.DefinitionByTitle(cboTitle.Text);
      slParams.Add(oDefinition.Definition);
    End
    Else If cboModel.Text <> '' Then
      slParams.Add(Format('--model=%s', [cboModel.Text]));

    If tsDrive.TabVisible Then
    Begin
      bHasA := AddDriveToCommand('a', cboDiskA, cboFormatA);
      If bHasA Then
      Begin
        AddDriveToCommand('b', cboDiskB, cboFormatB);
        AddDriveToCommand('c', cboDiskC, cboFormatC);
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
Begin

End;

Procedure TfrmMain.DiskorFormatChange(Sender: TObject);
Begin
  RefreshRC;
End;

End.
