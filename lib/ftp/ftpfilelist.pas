// This unit is the list of ftp fil download or upload for gui application
unit FtpFileList;

// This file is part of the Portabl and Pascal FTP Server
// Copyright (c) 2010 MARTINEAU Emeric.
//
// See the file license, included in this distribution,
// for details about the license.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes ;

type
  TFtpFile = record
      FileName : String ;
      Download : Boolean ;
      FtpClient : pointer ;
      Extra : pointer ;
  end ;

  PTFtpFile = ^TFtpFile ;

  TFtpFileList = class
  private
  protected
      // Array of TFtpFile
      FtpFileArray : array of TFtpFile ;
      // Capacity of array. When Nb = Capacity, it will be increase by ARRAY_RANGE
      Capacity : Integer ;
      // Number of item in array
      Nb : Integer ;

      // Get data
      function Get(const aiIndex : Integer) : TFtpFile ;
      // Put data
      procedure Put(const aiIndex : Integer; const arValue : TFtpFile) ;
  public
      // Default propery
      property Integers[Index: Integer]: TFtpFile read Get write Put; default;
      // Add item list
      procedure Add(const arValue : TFtpFile) ;
      // Clear list. All data is free.
      procedure Clear ;
      // Return count of item in list
      function Count : Integer ;
      // Constructor
      constructor Create ;
      // Constructor with initial capacity
      constructor Create(const aiInitialSize : Integer) ;
      // Free object. All data will be free
      destructor Destroy; override ;
      // Delete item. Item was free
      procedure Delete(const aiIndex : Integer) ;
      // Delete item. Item was free
      procedure Delete(const asValue : TFtpFile) ;
      // Insert list
      procedure Insert(const aiIndex : Integer; const arValue : TFtpFile) ;
      // Find a record
      function Find(const asFileName : String; const asDownload : Boolean;
          const asFtpClient : pointer) : Integer ;

  end ;

implementation

Const
    ARRAY_RANGE : Integer = 10 ;

//
// Constructor
constructor TFtpFileList.Create ;
begin
    inherited Create();

    Capacity := 0 ;
end ;

//
// Constructor with initial capacity
//
// @param aiInitialSize Initial capacity
constructor TFtpFileList.Create(const aiInitialSize : Integer) ;
begin
    Create ;

    Capacity := aiInitialSize ;

    SetLength(FtpFileArray, aiInitialSize) ;
end ;

//
//
// Free object. All data will be free
destructor TFtpFileList.Destroy ;
begin
    Clear ;

    inherited Destroy() ;
end ;

//
// Add item list
//
// @param arValue Item list to add
procedure TFtpFileList.Add(const arValue : TFtpFile) ;
begin
    try
        if Capacity = Nb
        then begin
            SetLength(FtpFileArray, Capacity + ARRAY_RANGE) ;

            Inc(Capacity, ARRAY_RANGE) ;
        end ;


        FtpFileArray[Nb] := arValue ;
        
        { On incrément après comme ça s'il y a une erreur, le count ne sera pas
          affecté }
        Inc(Nb) ;
    finally
    end ;
end ;

//
// Delete item. Item was free
//
// @param aiIndex Index of item
procedure TFtpFileList.Delete(const aiIndex : Integer) ;
var i : Integer ;
begin
    { -2 car la dernière cas va être supprimée }
    for i := aiIndex to Nb - 2 do
    begin
        FtpFileArray[i] := FtpFileArray[i + 1] ;
    end ;

    //FtpFileArray[nb] := nil ;
    
    Dec(Nb) ;

    if (Capacity - Nb) > ARRAY_RANGE
    then begin
        SetLength(FtpFileArray, Capacity - ARRAY_RANGE) ;

        Dec(Capacity, ARRAY_RANGE) ;
    end ;
end ;

//
// Delete item. Item was free
//
// @param asValue value to delete
procedure TFtpFileList.Delete(const asValue : TFtpFile) ;
var
    liIndex : Integer ;
begin
    for liIndex := 0 to Nb - 1 do
    begin
        if (FtpFileArray[liIndex].Download = asValue.Download) and
           (FtpFileArray[liIndex].Extra = asValue.Extra) and
           (FtpFileArray[liIndex].FileName = asValue.FileName) and
           (FtpFileArray[liIndex].FtpClient = asValue.FtpClient)
        then begin
            Delete(liIndex) ;

            break ;
        end ;
    end ;
end ;

//
// Return count of item in list
//
// @return count
function TFtpFileList.Count : Integer ;
begin
    Result := Nb ;
end ;

//
//
// Clear list. All data is free.
procedure TFtpFileList.Clear ;
begin
    SetLength(FtpFileArray, 0) ;

    Nb := 0 ;
end ;

//
// Get data
//
// @param aiIndex Index of item to get
//
// @return item
function TFtpFileList.Get(const aiIndex : Integer) : TFtpFile ;
begin
    Result := FtpFileArray[aiIndex] ;
end ;

//
// Put data
//
// @param aiIndex Index of item to put
// @param arValue value to add
procedure TFtpFileList.Put(const aiIndex : Integer;
    const arValue : TFtpFile) ;
begin
    FtpFileArray[aiIndex] := arValue ;
end ;

//
// Insert list
//
// @param aiIndex Index to insert
// @param arValue Item list to insert
procedure TFtpFileList.Insert(const aiIndex : Integer;
    const arValue : TFtpFile) ;
var liIndex : Integer ;
begin
    try
        if Capacity = Nb
        then begin
            SetLength(FtpFileArray, Capacity + ARRAY_RANGE) ;

            { On l'incrémente après comme ça en cas de problème il n'est pas
              incrémenté }
            Inc(Capacity, ARRAY_RANGE) ;
        end ;

        for liIndex := Nb downto (aiIndex + 1) do
        begin
            FtpFileArray[liIndex] := FtpFileArray[liIndex - 1] ;
        end ;

        FtpFileArray[aiIndex] := arValue ;

        { On incrément après comme ça s'il y a une erreur, le count ne sera pas
          affecté }
        Inc(Nb) ;
    finally
    end ;
end ;

//
// Find a record
//
// @param asFileName file name
// @param asDownload download
// @param asFtpClient ftp client
//
// @return index
function TFtpFileList.Find(const asFileName : String; const asDownload : Boolean;
    const asFtpClient : pointer) : Integer ;
var
    liIndex : Integer ;
begin
    Result := -1 ;

    for liIndex := 0 to Nb - 1 do
    begin
        if (FtpFileArray[liIndex].Download = asDownload) and
           (FtpFileArray[liIndex].FileName = asFileName) and
           (FtpFileArray[liIndex].FtpClient = asFtpClient)
        then begin
            Result := liIndex ;

            break ;
        end ;
    end ;
end ;

end.
