Unit CPMSupport;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Function IsCPMBootable(Const AInput: String): Boolean;
Function IsCPMBootableFile(Const AFilename: String): Boolean;
Function IsCPMBootableFolder(Const AFolder: String): Boolean;

Function DSKFormat(AInput: String): String;

Const
  FORMAT_UNKNOWN = 'FORMAT_UNKNOWN';

Implementation

Uses
  StringSupport, cpmtoolsSupport, FileUtil, Math;

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
  Result := False;

  If (FileExists(AFilename)) And (LowerCase(ExtractFileExt(AFilename)) = '.dsk') Then
  Begin
    b512 := 0;

    Assign(oFile, AFilename);
    Reset(oFile, 1);
    Try
      If FileSize(oFile) > 512 Then
      Begin
        Seek(oFile, 512);
        BlockRead(oFile, b512, 1);

        // Check if byte_at_512 = E5h
        Result := (b512 <> $E5);
      End
    Finally
      Close(oFile);
    End;
  End;
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
  iFinalUnderscore, iFinalDot, iEnd: Integer;
  sExt, sFilename: String;
Begin
  If DirectoryExists(AInput) Then
    Result := 'rcpmfs'
  Else
  Begin
    // For now, this is a simple filename parser.
    // In depth file analysis is a whole other project

    sExt := Lowercase(ExtractFileExt(AInput));
    If (sExt = '.dsk') Then
    Begin
      sFilename := ExtractFileName(AInput);
      sFilename := Copy(sFilename, 1, Length(sFilename) - Length(sExt));
      sFilename := Lowercase(sFilename);

      iFinalUnderscore := sFilename.LastIndexOf('_') + 1;
      iFinalDot := sFilename.LastIndexOf('.') + 1;

      If (iFinalUnderscore = 0) And (iFinalDot = 0) Then
        Result := FORMAT_UNKNOWN // diskname.dsk
      Else
      Begin
        // diskname_ss80.dsk
        // diskname.ss80.dsk
        iEnd := Max(iFinalUnderscore, iFinalDot);

        Result := Copy(sFilename, iEnd + 1, Length(sFilename));
      End;
    End
    Else If (sEXT <> '') Then
      Result := sExt // diskname.ss80
    Else
      Result := FORMAT_UNKNOWN; //diskname.other
  End;
End;

End.
