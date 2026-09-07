# Publishing furl

Canonical site: https://furl.farm/classic/

The `main` branch publishes at `/classic/`. Other source branches publish at `/classic/<branch>/`; slashes in branch names remain slashes in the URL. `classic`, `assets`, `live`, `img`, `style`, and `explorations` are reserved top-level branch names.

`Publish app` runs on source pushes and can also be run manually from Actions on the default branch, with a `branch` input. Workflow-only changes do not deploy automatically. Existing branches have the publishing workflow installed; new branches should inherit it from the default branch.

Builds are merged into `disconcision/furl-next`'s `gh-pages` branch. That is the single combined Pages site for this domain. A file manifest per app and branch lets an update remove its own stale assets while preserving every other app and branch. Concurrent pushes are retried against the latest combined tree; collisions fail before changing the site. Branch deletion does not automatically erase a published preview.

The repository secret `PAGES_DEPLOY_KEY` is an SSH deploy key with write access only to the combined site's repository. This avoids a personal access token and lets GitHub's branch-based Pages deployment trigger on artifact updates. Never print or commit the key. Shared publishing code and its tests live in `furl-next` on branch `furl`, under `.github/pages/`.

Personal-site URLs are redirects only. They must not receive app build artifacts again.
