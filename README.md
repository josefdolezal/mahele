# What is Mahele

Mahele is a language-agnostic code generation tool for model objects.
Have your team ever had to share same model objects between multiple platforms?
You did manually, didn't you?

With mahele, you define model layer of your app in platform-agnostic meta language - and you guess it - once.
"Next step?" you ask? Just use it everywhere.
Since all parts of an app shares the very same model, you can treat it as contract.

## Example

Currently, mahele supports `type`s and `enum`s.

Simple mahele model may be created as follows:

```mahele
type User =
    email: String
    name:  String?
    role:  UserRole

enum UserRole =
    | Superuser as 'admin'
    | User
```

For such schema, mahele will generate following model:

<details>
<summary>Swift</summary>

```swift
struct User {
    var email: String
    var name: String?
    var role: UserRole
}

enum Role: String {
    case superuser = "admin"
    case user
}
```

</details>

<details>
<summary>Haskell</summary>

```haskell
data User = User
    { email :: String
    , name :: Maybe String
    , role :: UserRole
    }

data Role = Superuser
          | User
}
```

</details>

<details>
<summary>Elm</summary>

```elm
type alias User =
    { email : String
    , name : Maybe String
    , role : UserRole
    }

type Role
    = Superuser
    | User
```

</details>

## Usage

### Available commands

<details>
<summary>Commands</summary>
</details>

### CLI

Mahele is primarly distributed as a CLI tool.
To create the model, you have two options:

- Download the schema from internet:

```bash
$ curl -L https://myre.po/schema.mahele | mahele 
```

- Use pre-downloaded local schema:

```bash
$ mahele create myschema.mahele 
```

Whenever possible, use the first option - this will help you to use latest schema on all platforms.

### Web interface

Want to give it a try before deploying mahele models into production?
You are not sure if mahele can handle your schema? No problemo!

There is an online version which you can try directly from your browser. Visit [mahele.online](mahele.online) and play around with your schema.

## Installation

<details>
<summary>Installation guide</summary>
Not available yet! :sad:
</details>
