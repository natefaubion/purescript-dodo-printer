let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.3-20210811/packages.dhall sha256:a2de7ef2f2e753733eddfa90573a82da0c7c61d46fa87d015b7f15ef8a6e97d5

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
