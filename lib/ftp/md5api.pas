{
 MD5Api - f0xi - 2006 - www.delphifr.com

 Modified by MARTINEAU Emeric 18/01/2008 delete unused function
}

unit MD5Api;

interface

{$IFDEF FPC}
    {$mode objfpc}{$H+}
{$ENDIF}

uses SysUtils;

{ ------------------------------------------------------------------------------------------------ }

type
  TMD5Data = array[0..15] of Byte;

{ ------------------------------------------------------------------------------------------------ }

// Renvois la representation du MD5 d'une chaine de caracteres
function MD5(const S : string) : string;

// Renvois une donnée MD5 d'un buffer quelquonque
function MD5DataFromBuffer(const Buffer; const Len: integer) : TMD5Data;

// Convertis une donnée MD5 vers une chaine de caracteres
function MD5DataToStr(const Data : TMD5Data) : string;

{ ------------------------------------------------------------------------------------------------ }

implementation

uses MD5Core;

{ ------------------------------------------------------------------------------------------------ }


function MD5(const S : string) : string;
begin
  result := MD5DataToStr( MD5DataFromBuffer(PChar(S)^, Length(S)) );
end;


{ ------------------------------------------------------------------------------------------------ }


function MD5DataFromBuffer(const Buffer; const Len: integer): TMD5Data;
var
  Context: TMD5Context;
begin
  MD5CoreInitialize(Context);
  MD5CoreUpdate(Context, Buffer, Len);
  Result := TMD5Data(MD5CoreFinalize(Context));
end;

{ ------------------------------------------------------------------------------------------------ }

function MD5DataToStr(const Data : TMD5Data) : string;
var
  P: PChar;
  I: Integer;
const
  Digits: array[0..15] of Char = '0123456789abcdef';
begin
  SetLength(result, 32);
  P := PChar(result);
  for I := 0 to 15 do begin
    P[0] := Digits[Data[I] shr 4];
    P[1] := Digits[Data[I] and $F];
    Inc(P,2);
  end;
end;

{ ------------------------------------------------------------------------------------------------ }
end.
