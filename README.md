**The Collector** pulls in data from 3rd party API services and stores it in CloudStore.

## Installation

**Requirements**

* [Cloudstore](https://github.com/airships/zephyr/tree/master/cloudstore)
* [Erlang R15B03](https://www.erlang-solutions.com/downloads/download-erlang-otp)

## Quick Start

Make sure CloudStore is running, then:

    git clone https://github.com/airships/collector
    ./apply_config
    make
    ./rel/collector/bin/collector console

## Configuration

Edit config/base to change port, keys, and CloudStore endpoint. Then:

    ./apply_config
    make

Then restart Collector:

    ./rel/collector/bin/collector stop
    ./rel/collector/bin/collector console

[Config documentation](https://github.com/airships/zephyr/wiki/Configuration)


## Usage (curl)

**Generate token for /users/bob**

    curl --request PUT --header "Content-Type: application/json" --data "{\/users/bob"\":\"rw\"}" --verbose http://127.0.0.1:10004/tokens/SECRET

**Generate a Facebook access token**

    TODO: show how

**Retrieve and Store Facebok Profile**

    curl --header "Content-Type: application/json" --header "Accept: application/json" --data "{\"service\":\"singly\", \"provider\":\"facebook\", \"request\": \"/profile\", \"access_token\": \"FACEBOOK_ACCESS_TOKEN\"}" --verbose --request PUT http://127.0.0.1:10005/users/bob?token=SECRET

**GET**

    curl --verbose http://127.0.0.1:10002/useres/bob?token=SECRET
