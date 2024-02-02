unit DialogDiskAlias;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  SynHighlighterIni, SynEdit;

type

  { TdlgDiskAlias }

  TdlgDiskAlias = class(TForm)
    ilMain: TImageList;
    memFile: TSynEdit;
    pcMain: TPageControl;
    pnlBottom: TPanel;
    pnlSummary: TPanel;
    Splitter1: TSplitter;
    SynIniSyn1: TSynIniSyn;
    tbMain: TToolBar;
    tsAliases: TTabSheet;
    tsDiskAlias: TTabSheet;
  private

  public

  end;

var
  dlgDiskAlias: TdlgDiskAlias;

implementation

{$R *.lfm}

end.

