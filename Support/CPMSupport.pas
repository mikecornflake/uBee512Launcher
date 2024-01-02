Unit CPMSupport;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Function IsCPMBootable(Const AInput: String): Boolean;
Function IsCPMBootableFile(Const AFilename: String): Boolean;
Function IsCPMBootableFolder(Const AFolder: String): Boolean;

Function DSKFormat(AInput: String): String;

Implementation

Uses
  StringSupport;

Function IsCPMBootable(Const AInput: String): Boolean;
Begin
  If FileExists(AInput) Then
    Result := IsCPMBootableFile(AInput)
  Else If DirectoryExists(AInput) Then
    Result := IsCPMBootableFolder(AInput)
  Else
    Result := False;
End;

Function IsCPMBootableFile(Const AFilename: String): Boolean;
Var
  oFile: File;
  b512: Byte;
Begin
  If (FileExists(AFilename)) And (LowerCase(ExtractFileExt(AFilename)) = '.dsk') Then
  Begin
    b512 := 0;

    Assign(oFile, AFilename);
    Reset(oFile, 1);

    Try
      Seek(oFile, 512);
      BlockRead(oFile, b512, 1);

      // Check if byte_at_512 = E5h
      Result := (b512 <> $E5);
    Finally
      Close(oFile);
    End;
  End
  Else
    Result := False;
End;

Function IsCPMBootableFolder(Const AFolder: String): Boolean;
Var
  sBootFile: String;
Begin
  sBootFile := IncludeTrailingPathDelimiter(AFolder) + '.libdsk.boot';
  Result := FileExists(sBootFile);
End;

Function DSKFormat(AInput: String): String;
Var
  iFinalUnderscore, iFinalDot: Integer;
Begin
  If DirectoryExists(AInput) Then
    Result := 'rcpmfs'
  Else
  Begin
    iFinalUnderscore := AInput.LastIndexOf('_') + 1;
    iFinalDot := AInput.LastIndexOf('.') + 1;

    If (iFinalUnderscore <> -1) And (iFinalDot > iFinalUnderscore) Then
      Result := Lowercase(Copy(AInput, iFinalUnderscore + 1, iFinalDot - iFinalUnderscore - 1))
    Else
      Result := '';
  End;
End;


End.
