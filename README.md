# Report Example

See [report.pdf](report.pdf) for documentation.

Note: To rename the project you need to replace all occurrences of "report" with your own library name in the following files:

- `hie.yaml`
- `package.yaml`


## How to contribute
- Checkout the latest commit on the `main` branch
- Create a new branch and make it the current branch. Prefix the branch name with your username or nickname, to prevent naming conflicts. 
    - Example command: `git checkout -b tiziano/example-branch`
- Make modifications to the code and commit the changes to your local repo. 
    - Example command: `git add -a && git commit -m "feature: added a definition for the Tableau data type."`
        - This command adds all the modified files to the list of files to commit (called "staging area"), and creates a new commit with those files.
    - Accurate commit messages are not necessary, as we are going to squash pull requests into the main branch; this also means that you should provide clear Pull Request titles and descriptions.
    - "Squashing" branches mean merging all of their commits, making a single commit for each pull request. That commit will be added to the PR's target branch. This avoids having merge commits in the main branch, which are usually as confusing as they're useless, and avoids the need to constantly rebasing branches using interactive rebase, which is not git-beginner-friendly.
- Push your modifications to the remote server.
    - Example command: `git push --set-upstream origin tiziano/example-branch`, where `tiziano/example-branch` is the name of your branch, and `origin` is the name of the remote server (it should be this by default, no worry about it).
- The result of this last command, if successful, will contain a URL that brings you to GitHub's UI to create a Pull Request in the project. Add a title, a decription, and confirm.
    - This will also automatically trigger a test run that will run the stack tests.
- You can review any pull requests, and merge them once everyone is on the same page.
- Strive for creating short PRs, often. If your work depends on an open PR, try to merge the older one first. Otherwise, contact Tiziano to figure out how to rebase/untangle your PR.
