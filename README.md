# Jungo Scripts

## Jungochain Runner

Enter dev environment:
```bash
nix develop
```

Build:
```bash
cabal build
```

Run:
```bash
cabal run jungochain-runner
```
Or
```bash
cabal run jungochain-runner -- [node-config-path] [spec-path]
```

Default node-config-path is `$PWD/.jungochain.yaml`
Default spec-path is `$PWD/.spec.json`
