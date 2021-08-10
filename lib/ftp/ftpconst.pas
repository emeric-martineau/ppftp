// This file contain all const
unit ftpconst;

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

uses
  Classes, SysUtils; 

const
     // Ftp eand-of-line
     FTP_EOL : String = #13#10 ;
     // YES
     YES_VALUE : String = 'yes' ;
     // YES
     NO_VALUE : String = 'no' ;
     // Separator of message
     MESSAGE_SEPARATOR : String = '\n' ;

     // Port to listen
     // @seealso(DEFAULT_PORT)
     MAIN_CONF_PORT : String = 'Port' ;
     // IP adress or name to listen
     // @seealso(DEFAULT_IP_ADDRESS)
     MAIN_CONF_IP_ADRESS : String = 'IPAddress' ;
     // Message to show at new client connection (\n to split in multiline)
     // @seealso(DEFAULT_WELCOME_MESSAGE)
     MAIN_CONF_WELCOME_MESSAGE : String = 'WelcomeMessage' ;
     // Message to show at log out (\n to split in multiline)
     // @seealso(DEFAULT_GOODBYE_MESSAGE)
     MAIN_CONF_GOODBYE_MESSAGE : String = 'GoodbyeMessage' ;
     // Maximum login per user
     // @seealso(DEFAULT_MAX_SESSION_USER)
     MAIN_CONF_MAX_SESSION_PER_USER : String = 'MaxSessionPerUser' ;
     // Maximum client
     // @seealso(DEFAULT_MAX_CLIENT)
     MAIN_CONF_MAX_CLIENT : String = 'MaxClient' ;
     // FullLog enable. If true, all ftp command are logged (yes/no)
     // @seealso(DEFAULT_FULL_LOG)
     MAIN_CONF_FULL_LOG : String = 'FullLog' ;
     // Time idle after client are deconnected
     // @seealso(DEFAULT_TIME_OUT)
     MAIN_CONF_TIME_OUT : String = 'TimeOut' ;
     // Range of passive port (min-max)
     // @seealso(DEFAULT_PASSIVE_PORT)
     MAIN_CONF_PASSIVE_PORT : String = 'PassivePort' ;
     // Deny adress are priority (yes/no)
     // @seealso(DEFAULT_DENY_PRIORITY)
     MAIN_CONF_DENY_PRIORITY : String = 'DenyPriority' ;
     // Allowed ip adress (or host)
     // @seealso(DEFAULT_ALLOW_ADDRESS)
     MAIN_CONF_ALLOW_IP_ADRESS : String = 'AllowIPAddress' ;
     // Deny ip adress (or host)
     // @seealso(DEFAULT_DENY_ADDRESS)
     MAIN_CONF_DENY_IP_ADRESS : String = 'DenyIPAddress' ;
     // Defaut buffer size for file transfert
     // @seealso(DEFAULT_BUFFER_SIZE)
     MAIN_CONF_BUFFER_SIZE : String = 'BufferSize' ;
     // Default byte rate for user
     // @seealso(DEFAULT_USER_BYTE_RATE)
     MAIN_CONF_USER_BYTE_RATE : String = 'UserByteRate' ;
     // If UTF8 support can be proposed to the client
     // @seealso(DEFAULT_UTF8)
     MAIN_CONF_UTF8 : String = 'Utf8Support' ;

     // Defaut port
     DEFAULT_PORT : Integer = 21 ;
     // Default ip adress
     DEFAULT_IP_ADDRESS : String = 'localhost' ;
     // Welcome Message
     DEFAULT_WELCOME_MESSAGE : String = 'Welcome to our server' ;
     // Goodbye message
     DEFAULT_GOODBYE_MESSAGE : String = 'Goodbye' ;
     // Maximum session per user
     DEFAULT_MAX_SESSION_USER : Integer = 5 ;
     // Maximum client
     DEFAULT_MAX_CLIENT : Integer = 512 ;
     // Default full log
     DEFAULT_FULL_LOG : Boolean = False ;
     // Default time out
     DEFAULT_TIME_OUT : Integer = 30 ;
     // Default passive port
     DEFAULT_PASSIVE_PORT : String = '49152-65534' ;
     // Default deny priority
     DEFAULT_DENY_PRIORITY : boolean = True ;
     // Default deny adress
     DEFAULT_ALLOW_ADDRESS : String = '' ;
     // Default deny adress
     DEFAULT_DENY_ADDRESS : String = '' ;
     // Default buffer size
     DEFAULT_BUFFER_SIZE : Integer = 512 ;
     // Default user byte rate
     DEFAULT_USER_BYTE_RATE : Integer = 2048 ;
     // Default utf8 support
     DEFAULT_UTF8 : Boolean = False ;

     // Default waiting time
     DEFAULT_WAITING_TIME : Integer = 1000 ;
     // Default block size
     DEFAULT_BLOCK_SIZE : Integer = 512 ;

     // Linger delay in millisecond
     LINGER_ENABLE : Boolean = True ;
     LINGER_DELAY : Integer = 10000 ;
     // Linger delay in millisecond for client connect
     LINGER_ENABLE_CLIENT : Boolean = True ;
     LINGER_DELAY_CLIENT : Integer = 1000 ;
     // Time out for read connection in millisecond
     READ_CONNECTION : Integer = 1000 ;

     // Password
     // @seealso(DEFAULT_USER_PASSWORD)
     USER_CONF_PASSWORD : String = 'Password' ;
     // Root directory
     // @seealso(DEFAULT_USER_ROOT)
     USER_CONF_ROOT : String = 'Root' ;
     // User download rigth
     // @seealso(DEFAULT_USER_DOWNLOAD)
     USER_CONF_DOWNLOAD : String = 'Download' ;
     // User upload right
     // @seealso(DEFAULT_USER_UPLOAD)
     USER_CONF_UPLOAD : String = 'Upload' ;
     // Rename right
     // @seealso(DEFAULT_USER_RENAME)
     USER_CONF_RENAME : String = 'Rename' ;
     // Delete right
     // @seealso(DEFAULT_USER_DELETE)
     USER_CONF_DELETE : String = 'Delete' ;
     // Make directory right
     // @seealso(DEFAULT_USER_MAKE_DIRECTORY)
     USER_CONF_MAKE_DIRECTORY : String = 'MakeDirectory' ;
     // Delete directory right
     // @seealso(DEFAULT_USER_DELETE_DIRECTORY)
     USER_CONF_DELETE_DIRECTORY : String = 'DeleteDirectory' ;
     // Show sub right
     // @seealso(DEFAULT_USER_SUB_DIR)
     USER_CONF_SUB_DIR : String = 'SubDir' ;
     // If user disabled
     // @seealso(DEFAULT_USER_DISABLED)
     USER_CONF_DISABLED : String = 'Disabled' ;
     // If user can modify file time
     // @seealso(DEFAULT_USER_MODIFY_FILE_TIME)
     USER_CONF_MODIFY_FILE_TIME : String = 'ModifyFileTime' ;

     // Default password. If empty, just list right
     DEFAULT_USER_PASSWORD : String = '' ;
     // Default root
     DEFAULT_USER_ROOT : String = '.' ;
     // Default download right
     DEFAULT_USER_DOWNLOAD : String = 'no' ;
     // Default upload
     DEFAULT_USER_UPLOAD : String = 'no' ;
     // Default rename right
     DEFAULT_USER_RENAME : String = 'no' ;
     // Default delete right
     DEFAULT_USER_DELETE : String = 'no' ;
     // Default make directory right
     DEFAULT_USER_MAKE_DIRECTORY : String = 'no' ;
     // Default delete directory right
     DEFAULT_USER_DELETE_DIRECTORY : String = 'no' ;
     // Default sub dir right
     DEFAULT_USER_SUB_DIR : String = 'no' ;
     // Default disabled user
     DEFAULT_USER_DISABLED : String = 'yes' ;
     // Default modify file time
     DEFAULT_USER_MODIFY_FILE_TIME : String = 'no' ;

     // FOLDER download rigth
     FOLDER_CONF_DOWNLOAD : String = 'Download' ;
     // FOLDER upload right
     FOLDER_CONF_UPLOAD : String = 'Upload' ;
     // Rename right
     FOLDER_CONF_RENAME : String = 'Rename' ;
     // Delete right
     FOLDER_CONF_DELETE : String = 'Delete' ;
     // Make directory right
     FOLDER_CONF_MAKE_DIRECTORY : String = 'MakeDirectory' ;
     // Delete directory right
     FOLDER_CONF_DELETE_DIRECTORY : String = 'DeleteDirectory' ;
     // Show sub right
     FOLDER_CONF_SUB_DIR : String = 'SubDir' ;
     // If FOLDER disabled
     FOLDER_CONF_DISABLED : String = 'Readable' ;
implementation

end.

