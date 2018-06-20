# fingerd

## Excercise 1

Add new user:

```shell
sudo sqlite3 finger.db
> insert into users values(Null, 'foo', '/bin/sh', '/home/foo', 'Foo Bar', '1234');
> select * from users;
1|gdp|/bin/sh|/home/gdp|Gaël Depreeuw|555-123-4567
2|foo|/bin/sh|/home/foo|Foo Bar|1234
```

Update existing user:

```shell
sudo sqlite3 finger.db
> update users set phone='n/a' where id=1
> select * from users;
1|gdp|/bin/sh|/home/gdp|Gaël Depreeuw|n/a
2|foo|/bin/sh|/home/foo|Foo Bar|1234
```

## Excercise 2

see [src/AddUser.hs](./src/AddUser.hs)