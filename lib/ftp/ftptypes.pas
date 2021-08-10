// This file contain all type
unit ftptypes;

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

//uses
//  Classes, SysUtils;

type
    // Procedure of log
    // @param asMessage message to log
    TLogProcedure = procedure(const asMessage : String) ;

    // Login/Logout call back
    //
    // @param asLogin login name
    // @param aiNumberOfLogin number of login
    TLoginLogoutProcedure = procedure(const asLogin : String;
        const aiNumberOfLogin : Integer) ;

    // Local config exists for folder
    //
    // @param asFolderName folder name
    // @param abUtf8 utf8 mode on
    // @param asKey key to read in local folder config
    // @param asValue return value in config file
    //
    // @return True if local folder config exists
    TFolderLocalConfigExists = function(const asFolderName : String;
        const abUtf8 : Boolean; const asKey : String;
        var asValue : String) : Boolean ;

    // Procedure to read main configuration
    //
    // @param asKey key to read
    //
    // List of values of asKey :
    //  @seealso(MAIN_CONF_PORT)
    //  @seealso(MAIN_CONF_IP_ADRESS)
    //  @seealso(MAIN_CONF_WELCOME_MESSAGE)
    //  @seealso(MAIN_CONF_GOODBYE_MESSAGE)
    //  @seealso(MAIN_CONF_MAX_SESSION_PER_USER)
    //  @seealso(MAIN_CONF_MAX_CLIENT)
    //  @seealso(MAIN_CONF_FULL_LOG)
    //  @seealso(MAIN_CONF_TIME_OUT)
    //  @seealso(MAIN_CONF_PASSIVE_PORT)
    //  @seealso(MAIN_CONF_DENY_PRIORITY)
    //  @seealso(MAIN_CONF_ALLOW_IP_ADRESS)
    //  @seealso(MAIN_CONF_DENY_IP_ADRESS)
    //  @seealso(MAIN_CONF_BUFFER_SIZE)
    //  @seealso(MAIN_CONF_USER_BYTE_RATE)
    TMainConfigReader = function(const asKey : String) : String ;

    // Type of user config
    TUserConfig = record
        Root : String ;
        Download : String ;
        Upload : String ;
        Rename : String ;
        Delete : String ;
        MakeDirectory : String ;
        DeleteDirectory : String ;
        ListSubDir : String ;
        Disabled : String ;
        ModifyFileTime : String ;
        UserFound : Boolean ;
    end ;

    // Procedure ro read user configuration
    //
    // @param asLoginName name of login
    // @param asKey key to read
    //
    // List of value of asKey
    //  @seealso(USER_CONF_PASSWORD)
    //  @seealso(USER_CONF_ROOT)
    //  @seealso(USER_CONF_DOWNLOAD)
    //  @seealso(USER_CONF_UPLOAD)
    //  @seealso(USER_CONF_RENAME)
    //  @seealso(USER_CONF_DELETE)
    //  @seealso(USER_CONF_MAKE_DIRECTORY)
    //  @seealso(USER_CONF_DELETE_DIRECTORY)
    //  @seealso(USER_CONF_SUB_DIR)
    //  @seealso(USER_CONF_DISABLED)
    TClientConfigReader = function(const asLoginName : String) : TUserConfig ;

    // check if password is good
    //
    // @param asLoginName login name
    // @param asPassword sending password
    //
    // @return True if password match ok
    TClientCheckPassword = function(const asLoginName : String; const asPassword : String) : Boolean ;

    // Login function call to know if we can login
    //
    // @param asLoginName login name
    //
    // @return True if can connect, False else (caus maximum login)
    TClientLogin = function(const asLoginName : String) : Boolean of object ;

    // Logout procedure
    //
    // @param asLoginName login name
    TClientLogout = procedure(const asLoginName : String) of object ;

    // Type of user config for ftp client
    TFtpClientConfig = record
        Login : String ;
        Root : String ;
        Download : Boolean ;
        Upload : Boolean ;
        Rename : Boolean ;
        Delete : Boolean ;
        MakeDirectory : Boolean ;
        DeleteDirectory : Boolean ;
        ListSubDir : Boolean ;
        Disabled : Boolean ;
        Connected : Boolean ;
        UserFound : Boolean ;
    end ;

    // Passive port
    PFtpPassivePort = ^TFtpPassivePort ;

    TFtpPassivePort = record
        Port : word ;
        Next : PFtpPassivePort ;
        Previous : PFtpPassivePort ;
    end ;

    // Get passive port
    //
    // @return port
    //
    // @seealso(TFreePassivePort)
    TGetPassivePort = function() : PFtpPassivePort of object ;

    // Free passive port
    //
    // @param asPort port return by TGetPassivePort
    //
    // @seealso(TGetPassivePort)
    TFreePassivePort = procedure(const asPort : PFtpPassivePort) of object ;

    // If file is protected and therefore we say doesn't exists
    //
    // @param asPathAndFileName complete file name with path
    // @param asUtf8 utf8 mode enabled
    TFileProtected = function(const asPathAndFileName : String;
        const abUtf8 : Boolean) : Boolean ;

    TTransfertMode = (tmBinary, tmAscii) ;
implementation

end.

