Unit FormDiskExplorer;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Types, Controls, Graphics, Dialogs, ShellCtrls,
  ExtCtrls, ComCtrls, StdCtrls, FormSettings;

Type

  { TfrmDiskExplorer }

  TfrmDiskExplorer = Class(TForm)
    btnAddDSKtoA: TToolButton;
    btnAddDSKtoB: TToolButton;
    btnAddDSKtoC: TToolButton;
    btnAddFolderToA: TToolButton;
    btnAddFolderToB: TToolButton;
    btnAddFolderToC: TToolButton;
    btnCancel: TButton;
    btnOK: TButton;
    ilMain: TImageList;
    lvcpmtoolsFiles: TListView;
    lvcpmtoolsWorkingFolder: TListView;
    memOutput: TMemo;
    Panel1: TPanel;
    pcPreview: TPageControl;
    pnlCPMToolsMain: TPanel;
    pnldskFiles: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    tsFiles: TTabSheet;
    tsText: TTabSheet;
    tvFolders: TShellTreeView;
    Procedure btnAddDSKtoAClick(Sender: TObject);
    Procedure btnAddDSKtoBClick(Sender: TObject);
    Procedure btnAddDSKtoCClick(Sender: TObject);
    Procedure btnAddFolderToAClick(Sender: TObject);
    Procedure btnAddFolderToBClick(Sender: TObject);
    Procedure btnAddFolderToCClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure lvcpmtoolsWorkingFolderSelectItem(Sender: TObject; Item: TListItem;
      {%H-}Selected: Boolean);
    Procedure tvFoldersChange(Sender: TObject; {%H-}Node: TTreeNode);
  Private
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
  If Assigned(lvcpmtoolsWorkingFolder.Selected) Then
    oItem := lvcpmtoolsWorkingFolder.Selected;

  If oItem.SubItems.Count > 2 Then
    Result := IncludeSlash(FSettings.WorkingFolder) + oItem.Caption + oItem.SubItems[0];
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
End;

Procedure TfrmDiskExplorer.btnAddDSKtoBClick(Sender: TObject);
Begin
  FSettings.B := SelectedFile;
End;

Procedure TfrmDiskExplorer.btnAddDSKtoCClick(Sender: TObject);
Begin
  FSettings.C := SelectedFile;
End;

Procedure TfrmDiskExplorer.btnAddFolderToAClick(Sender: TObject);
Begin
  FSettings.A := ExcludeSlash(tvFolders.Path);
End;

Procedure TfrmDiskExplorer.btnAddFolderToBClick(Sender: TObject);
Begin
  FSettings.B := ExcludeSlash(tvFolders.Path);
End;

Procedure TfrmDiskExplorer.btnAddFolderToCClick(Sender: TObject);
Begin
  FSettings.C := ExcludeSlash(tvFolders.Path);
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

      lvcpmtoolsWorkingFolder.Items.BeginUpdate;
      Try
        // Clear current folder file list
        lvcpmtoolsWorkingFolder.Clear;

        // Clear Preview
        memOutput.Lines.Clear;
        lvcpmtoolsFiles.Clear;

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

              oListItem := lvcpmtoolsWorkingFolder.Items.Add;
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
        lvcpmtoolsWorkingFolder.Items.EndUpdate;
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

  bFileSelected := Assigned(lvcpmtoolsWorkingFolder.Selected);

  If bFileSelected Then
  Begin
    oItem := lvcpmtoolsWorkingFolder.Selected;

    If oItem.SubItems.Count > 2 Then
    Begin
      bDSKBootable := oItem.SubItems[1] = 'Yes';
      bDSK := (oItem.SubItems[0] = '.dsk');
    End;
  End;

  btnAddDSKToA.Enabled := bDSK And bDSKBootable;
  btnAddDSKToB.Enabled := bDSK;
  btnAddDSKToC.Enabled := bDSK;

  btnAddFolderToA.Enabled := True;
  btnAddFolderToB.Enabled := True;
  btnAddFolderToC.Enabled := True;
End;

Procedure TfrmDiskExplorer.lvcpmtoolsWorkingFolderSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
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
      lvcpmtoolsFiles.Items.BeginUpdate;
      lvcpmtoolsFiles.Items.Clear;
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

              oListItem := lvcpmtoolsFiles.Items.Add;
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
      If (lvcpmtoolsFiles.Items.Count = 0) And (pcPreview.ActivePage = tsFiles) Then
        pcPreview.ActivePage := tsText;

      lvcpmtoolsFiles.Items.EndUpdate;
    Finally
      Log.DecIndent;
      Debug('End Preview: ' + sSelectedFile);

      ClearBusy;
    End;
  End;

  RefreshUI;
End;

End.
