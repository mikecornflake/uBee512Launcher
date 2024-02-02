Unit FormDiskExplorer;

{$mode ObjFPC}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
Interface

Uses
  Classes, SysUtils, Forms, Types, Controls, Graphics, Dialogs, ShellCtrls,
  ExtCtrls, ComCtrls, StdCtrls, Menus, FormSettings;

// TODO FormDiskExplorer: Add ability to use selected disk to update "disk.Alias"
// TODO FormDiskExplorer: Recognise if WorkingDirectory is selected, and make filenames relative
// TODO FormDiskExplorer: Add right click to MainListView

Type

  { TfrmDiskExplorer }

  TfrmDiskExplorer = Class(TForm)
    btnCancel: TButton;
    btnDiskAliasClear: TToolButton;
    btnOK: TButton;
    ilMain: TImageList;
    lvFilesInDisk: TListView;
    lvFiles: TListView;
    memOutput: TMemo;
    mnuA: TMenuItem;
    mnuB: TMenuItem;
    mnuC: TMenuItem;
    mnuEjectB: TMenuItem;
    mnuEjectC: TMenuItem;
    mnuInsertDiskB: TMenuItem;
    mnuInsertDiskC: TMenuItem;
    mnuInsertFolderB: TMenuItem;
    mnuInsertFolderC: TMenuItem;
    pmB: TPopupMenu;
    pmC: TPopupMenu;
    pmDiskAliasSet: TPopupMenu;
    pmDiskAliasClear: TPopupMenu;
    Separator1: TMenuItem;
    mnuEjectA: TMenuItem;
    mnuInsertDiskA: TMenuItem;
    mnuInsertFolderA: TMenuItem;
    Panel1: TPanel;
    pcPreview: TPageControl;
    pnlCPMToolsMain: TPanel;
    pnldskFiles: TPanel;
    pmA: TPopupMenu;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    ToolBar1: TToolBar;
    btnA: TToolButton;
    btnB: TToolButton;
    btnC: TToolButton;
    ToolButton1: TToolButton;
    btnDiskAliasSet: TToolButton;
    tsFiles: TTabSheet;
    tsText: TTabSheet;
    tvFolders: TShellTreeView;
    Procedure btnAddDSKtoAClick(Sender: TObject);
    Procedure btnAddDSKtoBClick(Sender: TObject);
    Procedure btnAddDSKtoCClick(Sender: TObject);
    Procedure btnAddFolderToAClick(Sender: TObject);
    Procedure btnAddFolderToBClick(Sender: TObject);
    Procedure btnAddFolderToCClick(Sender: TObject);
    Procedure btnDiskAliasClearClick(Sender: TObject);
    Procedure btnDiskAliasSetClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure lvFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    Procedure mnuEjectAClick(Sender: TObject);
    Procedure mnuEjectBClick(Sender: TObject);
    Procedure mnuEjectCClick(Sender: TObject);
    Procedure pmAPopup(Sender: TObject);
    Procedure pmBPopup(Sender: TObject);
    Procedure pmCPopup(Sender: TObject);
    Procedure pmDiskAliasClearPopup(Sender: TObject);
    Procedure pmDiskAliasSetPopup(Sender: TObject);
    Procedure tvFoldersChange(Sender: TObject; Node: TTreeNode);
    Procedure AssignDiskAlias(Sender: TObject);
  Private
    FSet: Boolean;

    FActivated: Boolean;
    FLoadingDSK: Boolean;
    FSettings: TSettings;

    Procedure RefreshUI;
    Function SelectedFile: String;
    Procedure SetSettings(AValue: TSettings);
  Public
    Property Settings: TSettings read FSettings write SetSettings;
  End;

Implementation

Uses
  FormMain, FileSupport, CPMSupport, StrUtils, Logs, OSSupport, ubee512Support,
  cpmtoolsSupport, LazFileUtils, StringSupport;

{$R *.lfm}

{ TfrmDiskExplorer }

Procedure TfrmDiskExplorer.FormCreate(Sender: TObject);
Begin
  FActivated := False;
  FLoadingDSK := False;
  FSettings := TSettings.Create;
End;

Procedure TfrmDiskExplorer.FormActivate(Sender: TObject);
Begin
  If Not FActivated Then
  Begin
    MakeFullyVisible;

    If DirectoryExists(FSettings.WorkingFolder) Then
      tvFolders.Path := FSettings.WorkingFolder;

    FActivated := True;
  End;

  RefreshUI;
End;

Procedure TfrmDiskExplorer.FormDestroy(Sender: TObject);
Begin
  FreeAndNil(FSettings);
End;

Function TfrmDiskExplorer.SelectedFile: String;
Var
  oItem: TListItem;
Begin
  Result := '';
  If Assigned(lvFiles.Selected) Then
  Begin
    oItem := lvFiles.Selected;
    If oItem.SubItems.Count > 2 Then
    Begin
      Result := IncludeSlash(FSettings.WorkingFolder) + oItem.Caption + oItem.SubItems[0];

      Result := uBee512.ShrinkFile('disks', Result);
    end;
  End;
End;

Procedure TfrmDiskExplorer.SetSettings(AValue: TSettings);
Begin
  Assert(Assigned(FSettings), 'TfrmSettings.FSetting not assigned');

  If Assigned(AValue) Then
    FSettings.Assign(AValue);
End;

Procedure TfrmDiskExplorer.btnAddDSKtoAClick(Sender: TObject);
Begin
  FSettings.A := SelectedFile;
  btnA.Hint := FSettings.A;
End;

Procedure TfrmDiskExplorer.btnAddDSKtoBClick(Sender: TObject);
Begin
  FSettings.B := SelectedFile;
  btnB.Hint := FSettings.B;
End;

Procedure TfrmDiskExplorer.btnAddDSKtoCClick(Sender: TObject);
Begin
  FSettings.C := SelectedFile;
  btnC.Hint := FSettings.C;
End;

Procedure TfrmDiskExplorer.btnAddFolderToAClick(Sender: TObject);
Begin
  FSettings.A := ExcludeSlash(tvFolders.Path);
  btnA.Hint := FSettings.A;
End;

Procedure TfrmDiskExplorer.btnAddFolderToBClick(Sender: TObject);
Begin
  FSettings.B := ExcludeSlash(tvFolders.Path);
  btnB.Hint := FSettings.B;
End;

Procedure TfrmDiskExplorer.btnAddFolderToCClick(Sender: TObject);
Begin
  FSettings.C := ExcludeSlash(tvFolders.Path);
  btnC.Hint := FSettings.C;
End;

Procedure TfrmDiskExplorer.btnDiskAliasClearClick(Sender: TObject);
Begin

End;

Procedure TfrmDiskExplorer.btnDiskAliasSetClick(Sender: TObject);
Begin

End;

Procedure TfrmDiskExplorer.mnuEjectAClick(Sender: TObject);
Begin
  FSettings.A := '';
  btnA.Hint := FSettings.A;
End;

Procedure TfrmDiskExplorer.mnuEjectBClick(Sender: TObject);
Begin
  FSettings.B := '';
  btnB.Hint := FSettings.B;
End;

Procedure TfrmDiskExplorer.mnuEjectCClick(Sender: TObject);
Begin
  FSettings.C := '';
  btnC.Hint := FSettings.C;
End;

Procedure TfrmDiskExplorer.tvFoldersChange(Sender: TObject; Node: TTreeNode);
Var
  oSearchRec: TSearchRec;
  oListItem: TListItem;
  bBoot, bIsDisk, bIsText: Boolean;
  sExt: Rawbytestring;
Begin
  If (FActivated) And Visible And (Not FLoadingDSK) Then
  Begin
    SetBusy;
    Try
      FLoadingDSK := True;
      FSettings.WorkingFolder := ExcludeSlash(tvFolders.Path);
      Debug('Start scanning folder: ' + FSettings.WorkingFolder);

      lvFiles.Items.BeginUpdate;
      Try
        // Clear current folder file list
        lvFiles.Clear;

        // Clear Preview
        memOutput.Lines.Clear;
        lvFilesInDisk.Clear;

        If FindFirst(IncludeSlash(FSettings.WorkingFolder) + '*.*', faAnyFile, oSearchRec) = 0 Then
          Repeat
            If (oSearchRec.Name <> '.') And (oSearchRec.Name <> '..') And
              (oSearchRec.Name <> '') Then
            Begin
              sExt := Lowercase(ExtractFileExt(oSearchRec.Name));
              bIsDisk := ubee512.IsDisk(sExt) Or cpmtoolsIsDisk(sExt);
              bIsText := IsTextfile(sExt);
              bBoot := bIsDisk And
                (IsCPMBootableFile(IncludeSlash(FSettings.WorkingFolder) + oSearchRec.Name));

              oListItem := lvFiles.Items.Add;
              oListItem.Caption := ExtractFileNameWithoutExt(oSearchRec.Name);
              oListItem.SubItems.Add(sExt);
              oListItem.SubItems.Add(BOOLEAN_YES_NO[bBoot]);
              oListItem.SubItems.Add(Format('%.1f', [oSearchRec.Size / 1024]));
              oListItem.SubItems.Add(DateTimeToStr(oSearchRec.TimeStamp));

              If bIsDisk Then
                oListItem.ImageIndex := DSK_ICO
              Else If bIsText Then
                oListItem.ImageIndex := TXT_ICO;
            End;
          Until FindNext(oSearchRec) <> 0;

        FindClose(oSearchRec);
      Finally
        lvFiles.Items.EndUpdate;

        If lvFiles.Items.Count > 0 Then
          lvFiles.Items[0].Selected := True;

        FLoadingDSK := False;
        Debug('End scanning folder: ' + FSettings.WorkingFolder);
      End;
    Finally
      ClearBusy;
    End;
  End;
End;

Procedure TfrmDiskExplorer.RefreshUI;
Var
  bFileSelected, bDSKBootable, bDSK: Boolean;
  oItem: TListItem;
Begin
  bDSKBootable := False;
  bDSK := False;

  bFileSelected := Assigned(lvFiles.Selected);

  If bFileSelected Then
  Begin
    oItem := lvFiles.Selected;

    If oItem.SubItems.Count > 2 Then
    Begin
      bDSKBootable := oItem.SubItems[1] = 'Yes';
      bDSK := (oItem.SubItems[0] = '.dsk');
    End;
  End;

  mnuInsertDiskA.Enabled := bDSK And bDSKBootable;
  mnuInsertDiskB.Enabled := bDSK;
  mnuInsertDiskC.Enabled := bDSK;

  mnuInsertFolderA.Enabled := True;
  mnuInsertFolderB.Enabled := True;
  mnuInsertFolderC.Enabled := True;

  btnA.Hint := FSettings.A;
  btnB.Hint := FSettings.B;
  btnC.Hint := FSettings.C;

  mnuA.Caption := FSettings.A;
  mnuB.Caption := FSettings.B;
  mnuC.Caption := FSettings.C;

  btnDiskAliasSet.Enabled := bDSK;
End;

Procedure TfrmDiskExplorer.lvFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
Var
  slTemp: TStringList;
  sSelectedFile, sDSKFile: String;
  arrStrings, arrFile: TStringDynArray;
  oListItem: TListItem;
  sRawOutput: String;
Begin
  If Assigned(Item) And (Item.SubItems.Count > 1) And
    (FileExists(IncludeSlash(FSettings.WorkingFolder) + Item.Caption + Item.SubItems[0])) And
    (Item.Selected) Then
  Begin
    SetBusy;
    sSelectedFile := IncludeSlash(FSettings.WorkingFolder) + Item.Caption + Item.SubItems[0];
    Debug('Start Preview: ' + sSelectedFile);
    Log.IncIndent;

    Try
      lvFilesInDisk.Items.BeginUpdate;
      lvFilesInDisk.Items.Clear;
      memOutput.Lines.Text := '';

      If Item.SubItems[0] = '.dsk' Then
      Begin
        slTemp := TStringList.Create;
        Try
          sRawOutput := '';

          slTemp.Text := cpmtoolsLS(sSelectedFile, sRawOutput);
          memOutput.Lines.Text := sRawOutput;

          For sDSKFile In slTemp Do
          Begin
            // 0: -rw-rw-rw- 10496 Jan 01 1970  access.box
            arrStrings := SplitString(sDSKFile, ' ');

            If Length(arrStrings) = 7 Then
            Begin
              arrFile := SplitString(arrStrings[6], '.');

              oListItem := lvFilesInDisk.Items.Add;
              oListItem.Caption := arrStrings[0];
              oListItem.SubItems.Add(arrFile[0]); // Filename
              oListItem.SubItems.Add(arrFile[1]); // Ext
              oListItem.SubItems.Add(arrStrings[1]); // Attr
              oListItem.SubItems.Add(arrStrings[2]); // Size
              oListItem.SubItems.Add(Format('%s %s %s',
                [arrStrings[3], arrStrings[4], arrStrings[5]]));  // Date
            End;
          End;
        Finally
          slTemp.Free;
        End;

        pcPreview.ActivePage := tsFiles;
      End
      Else If IsTextFile(Item.SubItems[0]) Then
      Begin
        slTemp := TStringList.Create;
        Try
          slTemp.LoadFromFile(sSelectedFile);

          memOutput.Lines.Text := slTemp.Text;
        Finally
          slTemp.Free;
        End;

        pcPreview.ActivePage := tsText;
      End;

      // There may be an error message the user needs to see
      If (lvFilesInDisk.Items.Count = 0) And (pcPreview.ActivePage = tsFiles) Then
        pcPreview.ActivePage := tsText;

      lvFilesInDisk.Items.EndUpdate;
    Finally
      Log.DecIndent;
      Debug('End Preview: ' + sSelectedFile);

      ClearBusy;
    End;
  End;

  RefreshUI;
End;

Procedure TfrmDiskExplorer.pmAPopup(Sender: TObject);
Var
  sSelected: String;
Begin
  sSelected := SelectedFile;
  If sSelected <> '' Then
    sSelected := ExtractFileName(sSelected);

  mnuA.Caption := FSettings.A;
  mnuInsertDiskA.Caption := Format('Insert disk "%s"', [sSelected]);
  mnuInsertFolderA.Caption := Format('Insert folder "%s"', [tvFolders.Path]);
End;

Procedure TfrmDiskExplorer.pmBPopup(Sender: TObject);
Var
  sSelected: String;
Begin
  sSelected := SelectedFile;
  If sSelected <> '' Then
    sSelected := ExtractFileName(sSelected);

  mnuB.Caption := FSettings.B;
  mnuInsertDiskB.Caption := Format('Insert disk "%s"', [sSelected]);
  mnuInsertFolderB.Caption := Format('Insert folder "%s"', [tvFolders.Path]);
End;

Procedure TfrmDiskExplorer.pmCPopup(Sender: TObject);
Var
  sSelected: String;
Begin
  sSelected := SelectedFile;
  If sSelected <> '' Then
    sSelected := ExtractFileName(sSelected);

  mnuC.Caption := FSettings.C;
  mnuInsertDiskC.Caption := Format('Insert disk "%s"', [sSelected]);
  mnuInsertFolderC.Caption := Format('Insert folder "%s"', [tvFolders.Path]);
End;

Procedure TfrmDiskExplorer.pmDiskAliasClearPopup(Sender: TObject);
Var
  oAlias: TDiskAlias;
  oMenu: TMenuItem;
Begin
  FSet := False;

  pmDiskAliasClear.Items.Clear;
  For oAlias In uBee512.DiskAliases Do
    If (oAlias.Alias <> '') And (oAlias.Filename <> '') Then
    Begin
      oMenu := TMenuItem.Create(pmDiskAliasSet);
      oMenu.Caption := Format('%s=%s', [oAlias.Alias, oAlias.Filename]);
      oMenu.OnClick := @AssignDiskAlias;

      pmDiskAliasClear.Items.Add(oMenu);
    End;
End;

Procedure TfrmDiskExplorer.pmDiskAliasSetPopup(Sender: TObject);
Var
  oAlias: TDiskAlias;
  oMenu: TMenuItem;
Begin
  FSet := True;

  pmDiskAliasSet.Items.Clear;
  For oAlias In uBee512.DiskAliases Do
    If (oAlias.Alias <> '') Then
    Begin
      oMenu := TMenuItem.Create(pmDiskAliasSet);
      oMenu.Caption := Format('%s=%s', [oAlias.Alias, oAlias.Filename]);
      oMenu.OnClick := @AssignDiskAlias;

      pmDiskAliasSet.Items.Add(oMenu);
    End;
End;

Procedure TfrmDiskExplorer.AssignDiskAlias(Sender: TObject);
Var
  oMenu: TMenuItem;
  sFilename, sAlias: String;
  oDiskAlias: TDiskAlias;
Begin
  If Sender Is TMenuItem Then
  Begin
    oMenu := TMenuItem(Sender);
    sAlias := Trim(ExtractWord(1, oMenu.Caption, ['=']));
    If FSet Then
      sFilename := SelectedFile
    Else
      sFilename := '';

    oDiskAlias := uBee512.DiskAliases[sAlias];
    If Assigned(oDiskAlias) Then
    Begin
      oDiskAlias.Filename := sFilename;
      oDiskAlias.Validator.Process;
    End;
  End;
End;

End.
