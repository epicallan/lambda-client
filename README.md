# lambda-runtime

An opinionated runtime environment for [Haskell] applications running on [AWS Lambda]. This library is a fork
and rewrite of the [nike/hal] library. It differs from the [nike/hal] library in the following ways:

- It only exposes a single function that constructs a lambda
- It has a strong emphasis on error catching and handling, exposing the ability to catch `LambdaClientErrors` to user applications for post processing and reporting.
- It aims for small dependence footprint in comparison to lambda-client through getting rid of conduit dependencies
- It doesn't have an internal retry mechanism in case of failed HTTP connections as found in [nike/hal].

## Table of Contents

  - [Supported Platforms / GHC Versions](#supported-platforms-ghc-versions)
  - [Quick Start](#quick-start)
  - [Local Testing](#local-testing)

## Supported Platforms / GHC Versions

We currently support this library under the same environment that [AWS Lambda
supports][lambda-env].

Our [CI] currently targets the latest two [GHC] versions under [stackage] (e.g. `8.10.x`, `8.8.x`).

If you haven't already, adding `docker: { enable: true }` to your `stack.yaml`
file will ensure that you're building a binary that can run in
[AWS Lambda][lambda-env].

## Quick Start

This quick start assumes you have the following tools installed:

  - [Stack][stack.yaml]
  - [Docker]
  - [aws-cli]

Add `lambda-client` to your [stack.yaml]'s [`extra-deps`] and enable
[Docker] integration so that your binary is automatically compiled in a
compatible environment for AWS. Also add `lambda-client` to your project's
dependency list (either `project-name.cabal` or `package.yaml`)

```yaml
#...
packages:
  - '.'
  - lambda-client-0.0.1
# ...
docker:
  enable: true
# ...
```

Then, define your types and handler:

```haskell
module Main where

import AWS.Lambda.Runtime
import Data.Aeson (FromJSON, parseJSON)
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Named = Named { name :: Text }
  deriving anyclass FromJSON
  deriving stock Generic

myHandler :: LambdaResponse -> IO (Either Text Text)
myHandler LambdaResponse{lambdaContext = LambdaContext{..}, ..} =
  case parseMaybe parseJSON event of
    Nothing -> return $ Left "My name is lambdaClient, what's yours?"
    Just (Named name) ->
      return . Right $ name <> " from " <> functionName static <> "!"

main :: IO ()
main = runtime myHandler

```

Your binary should be called `bootstrap` in order for the custom runtime
to execute properly:

```yaml
# Example snippet of package.yaml
# ...
executables:
  bootstrap:
    source-dirs: src
    main: Main.hs  # e.g. {project root}/src/Main.hs
# ...
```

Don't forget to define your [CloudFormation] stack:

```yaml
# file: template.yaml
AWSTemplateFormatVersion: '2010-09-09'
Transform: 'AWS::Serverless-2016-10-31'
Description: Test for the Haskell Runtime.
Resources:
  HelloWorldApp:
    Type: 'AWS::Serverless::Function'
    Properties:
      Handler: NOT_USED
      Runtime: provided
      # CodeUri is a relative path from the directory that this CloudFormation
      # file is defined.
      CodeUri: .stack-work/docker/_home/.local/bin/
      Description: My Haskell runtime.
      MemorySize: 128
      Timeout: 3
```

Finally, build, upload and test your lambda!

```bash
# Build the binary, make sure your executable is named `bootstrap`
stack build --copy-bins

# Create your function package
aws cloudformation package \
  --template-file template.yaml
  --s3-bucket lambda-client-test > \
  deployment_stack.yaml

# Deploy your function
aws cloudformation deploy \
  --stack-name "hello-world-haskell" \
  --region us-east-1 \
  --capabilities CAPABILITY_IAM \
  --template-file deployment_stack.yaml

# Take it for a spin!
aws lambda invoke \
  --function-name your_function_name \
  --region us-east-1 \
  --payload '{"name": "foo"}' \
  output.json
```

## Local Testing

### Dependencies

  - [Stack][stack.yaml]
  - [Docker]
  - [aws-sam-cli] (>v0.8.0)

### Build

```bash
docker pull fpco/stack-build:lts-{version} # First build only, find the latest version in stack.yaml
stack build --copy-bins
```

### Execute

```bash
echo '{ "accountId": "byebye" }' | sam local invoke --region us-east-1
```

[AWS Lambda]: https://docs.aws.amazon.com/lambda/latest/dg/welcome.html
[Haskell]: https://www.haskell.org/
[stack.yaml]: https://docs.haskellstack.org/
[`extra-deps`]: https://docs.haskellstack.org/en/stable/yaml_configuration/#yaml-configuration
[Docker]: https://www.docker.com/why-docker
[aws-cli]: https://aws.amazon.com/cli/
[CloudFormation]: https://aws.amazon.com/cloudformation/
[aws-sam-cli]: https://github.com/awslabs/aws-sam-cli
[lambda-env]: https://docs.aws.amazon.com/lambda/latest/dg/current-supported-versions.html
[ci]: https://travis-ci.org/Nike-Inc/lambda-client
[stackage]: https://www.stackage.org/
[GHC]: https://www.haskell.org/ghc/download.html
[nike/hal]: https://github.com/Nike-inc/hal
