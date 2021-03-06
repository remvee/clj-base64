[![Clojars Project](https://img.shields.io/clojars/v/clj-base64.svg)](https://clojars.org/clj-base64)

# clj-base64

Functions to encode and decode base64 strings.


## Usage

    (use 'remvee.base64)
    (encode-str "I 8> clojure!") ; => "SSA4PiBjbG9qdXJlIQ=="
    (decode-str "SSA4PiBjbG9qdXJlIQ==") ; => "I 8> clojure!"


## Installation

Drop the following dependency in your <code>project.clj</code> at the
appropriate place:

    [clj-base64 "0.0.3"]


## License

Copyright (c) Remco van 't Veer. All rights reserved.

The use and distribution terms for this software are covered by the Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at the root of this distribution.  By using this software in any fashion, you are agreeing to be bound by the terms of this license.  You must not remove this notice, or any other, from this software.
