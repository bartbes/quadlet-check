# Quadlet-check

Quadlet-check is a small utility that, given 1 or more podman quadlet files, checks whether they refer to the latest image.

This is useful when you want to have actual image versions in your quadlets, instead of just `latest`, but still want to be able to easily update, without having to check a bunch of container registries manually.

Currently, this utility is hardcoded to look for arm images (see `target_os_arch` in `main.ml`).

## Disclaimer

I mostly built this utility to try OCaml's Lwt.
That means I'm not that good at OCaml, and it's the first time I've tried Lwt.
Expect the code quality to match.

## Licence

This project is MIT-licensed.
See the LICENSE file for the exact text.
