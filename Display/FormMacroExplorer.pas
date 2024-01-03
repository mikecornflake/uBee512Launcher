Unit FormMacroExplorer;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls;

Type

  { TfrmMacroExplorer }

  TfrmMacroExplorer = Class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    lvSystemMacros: TListView;
    memRC: TMemo;
    memSystems: TMemo;
    Panel1: TPanel;
    pcRC: TPageControl;
    Splitter1: TSplitter;
    tsMacros: TTabSheet;
    tsRC: TTabSheet;
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure lvSystemMacrosSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  Private
    FActivated: Boolean;
    Procedure LoadRC;

  Public

  End;

Implementation

Uses
  uBee512Support, OSSupport;

{$R *.lfm}

Procedure TfrmMacroExplorer.FormCreate(Sender: TObject);
Begin
  FActivated := False;
End;

Procedure TfrmMacroExplorer.FormActivate(Sender: TObject);
Begin
  If Not Factivated Then
  Begin
    LoadRC;
    FActivated := True;
  End;
End;

Procedure TfrmMacroExplorer.lvSystemMacrosSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
Begin
  If (Selected) And (Assigned(Item)) Then
    memSystems.Lines.Text := uBee512MacroRC(Item.Caption)
  Else
    memSystems.Lines.Clear;
End;

Procedure TfrmMacroExplorer.LoadRC;
Var
  oMacro: TSystemMacro;
  oItem: TListItem;
Begin
  If uBee512Available And FileExists(uBee512RCPath) Then
  Begin
    SetBusy;
    Try
      memRC.Lines.LoadFromFile(uBee512RCPath);
      uBee512LoadRC;
      pcRC.ActivePage := tsMacros;

      lvSystemMacros.Items.Clear;
      memSystems.Lines.Clear;
      lvSystemMacros.Items.BeginUpdate;

      For oMacro In uBee512SystemMacros Do
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
    Finally
      lvSystemMacros.Items.EndUpdate;
      lvSystemMacros.AutoSize := True;
      ClearBusy;
    End;
  End;
End;


End.
