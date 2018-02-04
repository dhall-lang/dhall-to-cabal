{ type =
    [] : Optional ../types/RepoType.dhall 
, location =
    [] : Optional Text
, module =
    [] : Optional Text
, branch =
    [] : Optional Text
, tag =
    [] : Optional Text
, subdir =
    [] : Optional Text
, kind =
    (constructors ../types/RepoKind.dhall ).RepoHead {=}
}
