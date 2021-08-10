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
    TLogProcedure = procedure(const asMessage : String) ;

    // Login/Logout call back
    TLoginLogoutProcedure = procedure(const asLogin : String;
        const aiNumberOfLogin : Integer) ;

    // Procedure to read main configuration
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
        Password : String ;
        Root : String ;
        Download : String ;
        Upload : String ;
        Rename : String ;
        Delete : String ;
        MakeDirectory : String ;
        DeleteDirectory : String ;
        ListSubDir : String ;
        Disabled : String ;
        UserFound : Boolean ;
    end ;

    // Procedure ro read user configuration
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

    // Login function call to know if we can login
    // @param asLoginName login name
    //
    // @return True if can connect, False else (caus maximum login)
    TClientLogin = function(const asLoginName : String) : Boolean of object ;

    // Logout procedure
    // @param asLoginName login name
    TClientLogout = procedure(const asLoginName : String) of object ;

    // Type of user config for ftp client
    TFtpClientConfig = record
        Login : String ;
        Password : String ;
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

implementation

end.

