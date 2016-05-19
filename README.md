# AJK lomake 

## Modifying fields in the form

The form is declared as the `AJK` and related datatypes in
[`SatO.AJK.Lomake`](https://github.com/osakunta/ajk-lomake/blob/master/src/SatO/AJK/Lomake.hs).
Adding fields to form happens by adding fields to the data types.

## `supervisord.conf`

```ini
[program:ajk-lomake]
command=/usr/local/bin/ajk-lomake-server
autorestart=true
user=daemon
environment=PORT="8080",LOMAKE_EMAILADDR="foo@example.com",LOMAKE_HUOLTO_EMAILADDR="huolto@example.com";LANG="en_US.utf8"
```

## `apache.conf`

```xml
ProxyPass        /ajk-lomake http://localhost:8080
ProxyPassReverse /ajk-lomake http://localhost:8080
```
