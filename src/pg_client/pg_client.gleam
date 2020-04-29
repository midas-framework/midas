// gleam_pg/pg_client


import process/process.{Pid}

pub type PgoOptions {
  Host(String)
  Database(String)
  User(String)
  Password(String)
}

pub external fn start_link(List(PgoOptions)) -> Result(Pid(a), Nil) =
  "pg_client_native" "start_link"

pub type PgType{
    PgString(String)
    PgInt(Int)
}

pub type SqlReturn{
    Select(Int, List(List(PgType)))
    Insert(Int, List(List(PgType)))
}

pub external fn query(String, List(PgType)) -> Result(SqlReturn, Nil) =
  "pg_client_native" "query"
