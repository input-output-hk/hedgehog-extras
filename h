[33mcommit 0423cbec54e3abf0c1af2bd025969212587e53d4[m[33m ([m[1;36mHEAD -> [m[1;32mnewhoggy/new-assertion-functions[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Fri Jun 6 22:40:31 2025 +1000

    New assertion functions tryAssertion, assertFailure and assertFailure_

[33mcommit 332c76886bd7d31d1ef7dfcab238c66343243fad[m[33m ([m[1;31morigin/main[m[33m, [m[1;31morigin/HEAD[m[33m, [m[1;32mmain[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Thu Jun 5 21:05:09 2025 +1000

    Re-export Golden module from Test module

[33mcommit 7852922752b194db075047110bf4d3b1fb43371b[m
Author: John Ky <john.ky@iohk.io>
Date:   Thu Jun 5 21:03:08 2025 +1000

    Add diffVsGoldenFileExcludeTrace function

[33mcommit 61b2b125808ecdb52ec976abff471abec1d17a3e[m
Author: John Ky <john.ky@iohk.io>
Date:   Thu Jun 5 21:02:08 2025 +1000

    Thread safe golden test support

[33mcommit e7ae50af28075f0e6f7baab264a4bd479f757445[m
Author: John Ky <john.ky@iohk.io>
Date:   Thu Jun 5 20:47:13 2025 +1000

    New UnitIO monad

[33mcommit 5998a497c7be2a085b77394fbcd6fc913a33435b[m
Author: John Ky <john.ky@iohk.io>
Date:   Thu Jun 5 20:33:13 2025 +1000

    Move orphans to own file

[33mcommit f4e5f08024323dcadf2a97797b5dcfbbe481016c[m
Author: Nicolas BACQUEY <nicolas.bacquey@tweag.io>
Date:   Mon Jun 2 14:27:39 2025 +0200

    Fix haddock for `byDeadline` and `byDuration`

[33mcommit 13ee0104d25a0843a6d978aa441e9af4aeeb28e8[m
Author: Nicolas BACQUEY <nicolas.bacquey@tweag.io>
Date:   Wed Apr 23 15:18:46 2025 +0200

    Fix haddock for `threadDelay`
    
    `threadDelay` calls `Control.Concurrent.threadDelay`, which takes an
    argument in microseconds, instead of milliseconds as the documentation
    specified. This commit fixes that typo.

[33mcommit 84ae0ba0413d613d12bb16a864187f5cc624a232[m
Author: Pablo Lamela <pablo.lamela@iohk.io>
Date:   Wed Feb 5 16:23:35 2025 +0100

    Define `expectFailure` in terms of `expectFailureWith`

[33mcommit d69534aca6bd33a3e15695f21d9a7bbb6f794971[m
Author: Pablo Lamela <pablo.lamela@iohk.io>
Date:   Mon Jan 20 18:10:40 2025 +0100

    Add `expectFailureWith` combinator and tests

[33mcommit d25a04aa9ffd23694163abb7f644655b2fc20eb7[m
Author: Pablo Lamela <palas87@gmail.com>
Date:   Wed Oct 16 15:29:23 2024 +0200

    Generalize resulting monad in `expectFailure`
    
    Co-authored-by: Mateusz Galazyn <228866+carbolymer@users.noreply.github.com>

[33mcommit 64650e830a698ec9df88897434ed462da568557e[m
Author: Pablo Lamela <pablo.lamela@iohk.io>
Date:   Tue Sep 24 17:49:49 2024 +0200

    Add `expectFailure` combinator

[33mcommit 113924613863e35bf2f7c901b3bbbae99d18d29f[m
Author: Clément Hurlin <clement.hurlin@moduscreate.com>
Date:   Fri Mar 21 09:39:04 2025 +0100

    Make the GitHub page workflow manually triggerable (workflow_dispatch)

[33mcommit 4a0d2935f203c5fcdf92150a8a4b68525106fe43[m
Author: Clément Hurlin <clement.hurlin@moduscreate.com>
Date:   Wed Mar 19 16:35:00 2025 +0100

    Detail: fix a warning in the Haskell pipeline

[33mcommit 97cac60aef944a39ba3dbee390c4a7e13df979a4[m
Author: Clément Hurlin <clement.hurlin@moduscreate.com>
Date:   Wed Mar 19 15:22:13 2025 +0100

    Align haddock pipeline with Haskell pipeline

[33mcommit ee17d4c22a49f7fdfb2de4db406f731fd74bfd9c[m
Author: Clément Hurlin <clement.hurlin@moduscreate.com>
Date:   Tue Mar 18 16:57:27 2025 +0100

    Process: more informative error message when plan.json does not exist

[33mcommit 34ca73f1e12ca3475110ee46b2628b956f81a003[m
Author: Clément Hurlin <clement.hurlin@moduscreate.com>
Date:   Wed Mar 19 14:18:06 2025 +0100

    Loosen base version, to allow building with GHC 9.12

[33mcommit 83e26c485464377367e71a9bc5776fc8ed8736f0[m
Author: Clément Hurlin <clement.hurlin@moduscreate.com>
Date:   Wed Mar 19 10:17:20 2025 +0100

    Haskell CI: simplify build workflow, using cardano-cli's CI as guidance

[33mcommit e3d048847277b648ce1a8f4f822d4b7c46915fb1[m
Author: Clément Hurlin <clement.hurlin@moduscreate.com>
Date:   Wed Mar 19 10:17:03 2025 +0100

    Haskell CI: reduce number of shellcheck warnings

[33mcommit a8120b84d1baa4b26c69668fc37f307ba9e942e1[m[33m ([m[1;33mtag: v0.7.0.0[m[33m)[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Tue Dec 24 11:54:28 2024 +0100

    New version hedgehog-extras-0.7.0.0
    
    0.6.5.1 was deprecated because it contained breaking changes. This is
    just a version bump aligning with Haskell PVP.

[33mcommit 0d9784e29a2b0d2514a019341a1b498ab5b8e9ef[m[33m ([m[1;33mtag: v0.6.5.1[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed Nov 20 22:32:45 2024 +1100

    New version hedgehog-extras-0.6.5.1

[33mcommit bdd89f779248140812424711fee6585c29757f6d[m
Author: Jordan Millar <jordan.millar@iohk.io>
Date:   Tue Nov 19 15:22:56 2024 -0400

    Enforce UTF-8 encoding on reading and writing files

[33mcommit 40bd9e507089356489c1e86923d9258f80d758e9[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Fri Oct 11 13:10:43 2024 +0200

    Improve error reporting when calling binaries from plan.json

[33mcommit b19883671bb034f3fb596c968be9efc78d68a1e5[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Fri Oct 11 12:26:47 2024 +0200

    Make workspace cleanup retry on failure

[33mcommit 40da3c7c63a0ab76e33473b641e88196141de38f[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Fri Oct 11 11:46:19 2024 +0200

    Update GH actions artifact to v4

[33mcommit 3eacfaceb561deb74124e08f596429a7eb9c9106[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Fri Oct 11 11:42:14 2024 +0200

    Add pretty annotating functions

[33mcommit 2a7d307f52da34ca55e2b20cc6957dd175741574[m[33m ([m[1;33mtag: v0.6.5.0[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Sat Jul 27 18:44:42 2024 +1000

    New version hedgehog-extras-0.6.5.0

[33mcommit 0496d992f347d6755cf7e8d6f0c36bc6ff8a3422[m
Author: Erik de Castro Lopo <erikd@mega-nerd.com>
Date:   Fri Jul 26 15:31:41 2024 +1000

    CI: Add ghc-9,.10 to the build matrix

[33mcommit 1d4468ce4e74e7a4b3c1fec5c1b21360051a3e72[m
Author: Erik de Castro Lopo <erikd@mega-nerd.com>
Date:   Fri Jul 26 15:15:21 2024 +1000

    Make it build with ghc-9.10
    
    Also update the version of `tar` being used.

[33mcommit cfc88159aebabdf93e1ad1ae216178a5bf3b47d9[m[33m ([m[1;33mtag: v0.6.4.0[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Thu May 16 21:57:12 2024 +1000

    New version hedgehog-extras-0.6.4.0

[33mcommit 93b7480c8541c0b9db9bc2d518801fff331cbfe3[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Wed May 15 16:48:40 2024 +0200

    Add missing `HasCallStack` to writeGoldenFile, reportGoldenFileMissing, checkAgainstGoldenFile

[33mcommit 4614d6503899ed90a561a5da81d7806745bb94c2[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Fri May 10 10:10:14 2024 +0200

    Use removePathForcibly to remove directories

[33mcommit 7a1c4e2f459e6f2357d2583a40215279e248db2c[m[33m ([m[1;33mtag: v0.6.3.0[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Fri May 3 23:09:19 2024 +1000

    New version 0.6.3.0

[33mcommit e43586907f7b90e6b5a97b11144cea9b4a1de38f[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Wed May 1 17:21:14 2024 +0200

    Add test for asyncRegister_

[33mcommit ed8060aaa87d6ba15e60d25a84a52f5c2e8d6edc[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Thu Apr 25 17:18:02 2024 +0200

    Add TestWatchdog and Tripwire with their tests

[33mcommit 7f1c5a928e3addbd3af7b40ed825b0423e900b68[m
Author: John Ky <john.ky@iohk.io>
Date:   Fri May 3 21:06:52 2024 +1000

    Allow the port to be reused immediately after it is closed

[33mcommit 285f1dfb9c4e9c935f4244d3df988bc5d0c84c21[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Apr 30 22:28:11 2024 +1000

    Switch to using haskell-actions/setup

[33mcommit ff7d2f03e95a26678d82b85df02720b114d4d3a1[m[33m ([m[1;33mtag: v0.6.2.0[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Apr 23 18:37:46 2024 +1000

    New version hedgehog-extras-0-.6.2.0

[33mcommit 2c75711a40629a7a6c0b16f7cdec9e0be406284f[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon Apr 22 23:35:06 2024 +1000

    New randomPort, reserveRandomPort and portInUse functions

[33mcommit 968ef1406dc22b5854a63b1893f15a9b40641559[m
Author: Pablo Lamela <pablo.lamela@iohk.io>
Date:   Wed Mar 20 19:24:14 2024 +0100

    Apply suggestions from code review
    
    Co-authored-by: Mateusz Galazyn <228866+carbolymer@users.noreply.github.com>

[33mcommit f2e3ccb2abb9c325180fad6fa293aab81ce09259[m
Author: Pablo Lamela <pablo.lamela@iohk.io>
Date:   Wed Mar 20 16:24:17 2024 +0100

    Remove double space and add quotes

[33mcommit 95117ed85ccb508f66645077f2f34f4a1b79fa3e[m
Author: Pablo Lamela <pablo.lamela@iohk.io>
Date:   Wed Mar 20 16:18:20 2024 +0100

    Apply suggestions from code review
    
    Co-authored-by: Mateusz Galazyn <228866+carbolymer@users.noreply.github.com>

[33mcommit 4aedf6ab7baaa90d7acc3cf063c7ec9210a40e25[m
Author: Pablo Lamela <pablo.lamela@iohk.io>
Date:   Mon Mar 18 23:08:14 2024 +0100

    Add test to ensure a directory exists (`assertDirectoryExists`)

[33mcommit dc49b0510df4cff520f25d9339c061202c13c81f[m
Author: Pablo Lamela <pablo.lamela@iohk.io>
Date:   Wed Mar 13 17:18:40 2024 +0100

    Add test to ensure a directory doesn't exist (`assertDirectoryMissing`)

[33mcommit 56cb83973b39a08acda7408e97d1be3a33443d73[m[33m ([m[1;33mtag: v0.6.1.0[m[33m)[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Tue Feb 13 19:15:29 2024 +0100

    New version hedgehog-extras-0.6.1.0

[33mcommit 7c0b9e53852ea0fb6a7e4fc998b92c7e9fbc0e20[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Tue Feb 13 19:08:46 2024 +0100

    #39 Lower bound on aeson

[33mcommit 7d48cfa714c559016aef9cc5377b416adfb6e263[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Tue Feb 13 18:46:56 2024 +0100

    Lower aeson bound

[33mcommit 25fecf8e25bdb7efb45bd6ce89e73a1aef1b070c[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Wed Feb 7 11:36:59 2024 +0100

    Allow reading files into any FromJSON

[33mcommit 8a40189abca6fd2f3aa0981596880b9ecbf3c11d[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Fri Feb 9 15:38:04 2024 +0100

    Remove double printing of a command with its arguments

[33mcommit be4c0be656187999bfdbc42310febe94b529c2f6[m[33m ([m[1;33mtag: v0.6.0.2[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon Jan 29 21:07:58 2024 +1100

    New version hedgehog-extras-0.6.0.2

[33mcommit e499136bb4d700f1d305166404d0742c4577eab3[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Fri Jan 26 08:15:43 2024 +0100

    Fix missing call sites for short-circuiting functions

[33mcommit a7bb132183242fbfba371d9161bfce9d1f4e3607[m[33m ([m[1;33mtag: v0.6.0.1[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Jan 23 19:18:35 2024 +1100

    New version hedgehog-extras-0.6.0.1

[33mcommit 3ab41d973aef114528da41d4c3b2cf0ea60f27dd[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Mon Jan 22 16:55:59 2024 +0100

    Fix missing stderr in failed commands

[33mcommit 13509fb0d83a123cb6e075c3346256d93fb40130[m[33m ([m[1;33mtag: v0.6.0.0[m[33m)[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Tue Jan 16 10:10:28 2024 +0100

    New version 0.6.0.0

[33mcommit 5874f504e9a236b5b1877e61db373fec132915d3[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Thu Jan 11 17:16:01 2024 +0100

    Add concurrency abstractions from lifted-async and lifted-base

[33mcommit 06f32cdf0a0826da2a7fe07908956d09ef841fdc[m
Author: Clément Hurlin <smelc@users.noreply.github.com>
Date:   Thu Jan 11 11:26:07 2024 +0100

    Add exec variant that allows a negative call (#55)
    
    * Add a exec variant that doesn't fail upon receiving a non-zero error code
    
    * exec*: shorten output (to enhance readibility)
    
    * Add a execFlex variant that doesn't fail upon receiving a non-zero exit code
    
    * New exec variants: add argument-level documentation

[33mcommit fdbed5a2344eb510f6a20c39813e7bcf2aa96776[m[33m ([m[1;33mtag: v0.5.1.0[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Thu Jan 4 00:51:29 2024 +1100

    New version 0.5.1.0

[33mcommit b09f7cc2bf94297c1c9074378393632a36936ed8[m
Author: John Ky <john.ky@iohk.io>
Date:   Thu Jan 4 00:39:29 2024 +1100

    Use MultiwayIf to simplify code

[33mcommit 1b41ea322a005681775d98b6f0b1988677161b00[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed Jan 3 21:35:08 2024 +1100

    New RECREATE_GOLDEN_FILES which will causes golden tests to always create golden files replacing any existing golden files

[33mcommit 0016f0be6522e160184f58de035abc40d5bf6531[m
Author: John Ky <john.ky@iohk.io>
Date:   Sat Dec 16 22:25:06 2023 +1100

    Add upper bound to tar

[33mcommit 1ec284b5e728e2beabda7512a5e76eae8b667ebb[m[33m ([m[1;33mtag: v0.5.0.0[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Thu Nov 23 20:28:59 2023 +1100

    New version 0.5.0.0

[33mcommit 830c20137265383588a61026632d2496bb61226d[m[33m ([m[1;33mtag: v0.4.8.0[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Nov 21 21:39:50 2023 +1100

    New version hedgehog-extras-0.4.8.0

[33mcommit e8ae834c205ec91eafddbc23420ae4d7749b6466[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Nov 21 21:23:55 2023 +1100

    Remove unused import

[33mcommit 3838c625fc5fb9301a3c782f7b5a7c07c53a5ede[m
Author: Erik de Castro Lopo <erikd@mega-nerd.com>
Date:   Tue Nov 21 16:29:47 2023 +1100

    CI: Add ghc-9.8.1 to build matrix

[33mcommit aa066740c825f5f9f3dc0e24c47fb1b5ae1ca4b4[m
Author: Erik de Castro Lopo <erikd@mega-nerd.com>
Date:   Tue Nov 21 16:25:12 2023 +1100

    Make it build with ghc-9.8
    
    Drop hw-aeson dependency on the way.

[33mcommit 47df6f426d2b8fb01c66c279a818930eb8e5cd3f[m[33m ([m[1;33mtag: v0.4.7.1[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Thu Oct 5 11:33:07 2023 +1100

    New version hedgehog-extras-0.4.7.1

[33mcommit 42505e3abace11782fccd6ebc4e12d92cd856206[m
Author: John Ky <john.ky@iohk.io>
Date:   Thu Oct 5 10:09:24 2023 +1100

    Remove dependency on hw-aeson

[33mcommit 28dab323f721f83d8d248543b30ce04e3e1f7527[m[33m ([m[1;33mtag: v0.4.7.0[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Jun 27 14:17:36 2023 +1000

    New version 0.4.7.0

[33mcommit ab621305b48217127e30666adc7bec0c80ec223f[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Jun 27 10:34:18 2023 +1000

    New indexM function

[33mcommit 1901ae8211b0d1496dfedec4abb7a72821f3826b[m[33m ([m[1;33mtag: v0.4.6.0[m[33m)[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Mon Jun 19 15:02:15 2023 +0200

    New version 0.4.6.0

[33mcommit 0db485cfd868458046b7cc61c9f1451f8fb8ec3e[m
Author: Mateusz Gałażyn <228866+carbolymer@users.noreply.github.com>
Date:   Mon Jun 19 14:51:38 2023 +0200

    Create new process groups for newly spawned processes (#42)

[33mcommit 6c9e22998467d0889946a1517e8bea65c44b016b[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue May 30 22:56:38 2023 +1000

    Add a way to log golden files

[33mcommit 98ab0cf010b90a10cba632fe7ea19feb6b5909a2[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed May 10 20:39:25 2023 +1000

    Put the golden-file filename in the failure message of diffVsGoldenFile

[33mcommit e7f3c9ff967ed6f3b4c8c17013a4e0c2f541e053[m[33m ([m[1;33mtag: v0.4.5.1[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed May 10 20:33:50 2023 +1000

    New version 0.4.5.1

[33mcommit 0034fb4298a1e10b42537e973c2e13e5a1d371dd[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed May 10 20:24:08 2023 +1000

    Fix empty case for diffVsGoldenFile

[33mcommit f1fc306015ff82c5995f485b30226030503f2ca6[m[33m ([m[1;33mtag: v0.4.5.0[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue May 9 18:52:22 2023 +1000

    New version 0.4.5.0

[33mcommit d85709d8a29e119d57524ac7dfd7d8e25744bbb7[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon May 8 20:39:00 2023 +1000

    New downloadAndExtractGithubCommitToTemp

[33mcommit 963945fae03ef9695c6aad6b95ff20b82d5b2e09[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon May 8 16:59:18 2023 +1000

    New downloadToFile function

[33mcommit 68f85c36e67a62c7dd79c62e937931dd44465ee6[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon May 8 17:24:47 2023 +1000

    Fix annotation for renameFile

[33mcommit 8afa8e8bdf0f6516f616738a7804b4cf63615c05[m[33m ([m[1;33mtag: v0.4.4.1[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Fri May 5 15:21:37 2023 +1000

    New version 0.4.4.1

[33mcommit 852398f6526661ef58f6f8203f4401aa9e2ef2a2[m
Author: Erik de Castro Lopo <erikd@mega-nerd.com>
Date:   Fri May 5 12:25:42 2023 +1000

    CI: Add ghc-9.6.1 to build matrix

[33mcommit 14a47f45c6603aecc9c838948d2fe984ff9c3328[m
Author: Erik de Castro Lopo <erikd@mega-nerd.com>
Date:   Fri May 5 12:23:58 2023 +1000

    Make it build with ghc-9.6

[33mcommit eb2350cea084679d52a17c93030ae7699b068e5a[m[33m ([m[1;33mtag: v0.4.4.0[m[33m)[m
Author: Mateusz Gałażyn <228866+carbolymer@users.noreply.github.com>
Date:   Thu May 4 16:04:56 2023 +0200

    Add extra assertion functions, fix readM to show correct line. Version 0.4.4. (#33)

[33mcommit 040d74b27ecb0b709c47002ebca947cabde0e667[m[33m ([m[1;33mtag: v0.4.3.0[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed May 3 01:32:45 2023 +1000

    Tidy up imports

[33mcommit e11a2f43a212b7846bd28d3f4aff783e71714086[m
Author: John Ky <john.ky@iohk.io>
Date:   Sat Apr 29 10:30:18 2023 +1000

    New Hedgehog.Extras.Test.Golden module

[33mcommit 72cc8a32a702ac4dbdedee8d7299c2825a0c465e[m
Author: Mateusz Galazyn <mateusz.galazyn@iohk.io>
Date:   Tue May 2 15:09:17 2023 +0200

    Add assertWithinTolerance and readM. Version bump.

[33mcommit 8a9c79b721387d25b54f414df64538298c8b75aa[m[33m ([m[1;33mtag: v0.4.2.0[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Fri Apr 28 23:54:57 2023 +1000

    New version 0.4.2.0

[33mcommit 234ed5e3987dbd382eef71be387215312a7fd51b[m
Author: John Ky <john.ky@iohk.io>
Date:   Fri Apr 28 23:48:03 2023 +1000

    New functions assertFileExists assertFileMissing assertFilesMissing

[33mcommit 9efa1f168e6fd67f6d07abe292d759ae9c6cc7be[m
Author: John Ky <john.ky@iohk.io>
Date:   Fri Apr 28 23:24:06 2023 +1000

    Delete assertFileExists from Network module because it doesn't belong here

[33mcommit e5fd7b988e7629547b607ed3551dcb1aa49c5ce1[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed Apr 12 16:41:20 2023 +1000

    Use filepath operator (</>) over (<>) with "/"

[33mcommit 6796d6350af809c2640924ede1993579de2ad164[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed Apr 12 16:35:11 2023 +1000

    New version 0.4.1.0

[33mcommit 542257697f5780dd0d5e9064fc9358fd74983f98[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed Apr 12 14:53:30 2023 +1000

    New createSubdirectoryIfMissing function

[33mcommit 63a781d782fd25de4f1af44da5ea69a90132ff9f[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed Apr 12 14:22:48 2023 +1000

    New Ok versions of functions that require the operation to succeed

[33mcommit 4ae1f23fdbd0842c3db4aee7022b5399e1a2c7e0[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed Apr 12 14:14:36 2023 +1000

    Fix compile errors

[33mcommit 10beb5e6e5d3d605dc3873e4c66d36a0671db5e8[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed Apr 12 14:03:22 2023 +1000

    Modify createDirectoryIfMissing to return its argument.  createDirectoryIfMissing_ will be the version that returns ()

[33mcommit 81053613356ca3a30a5a9b867e0a5c643bf9309b[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon Mar 6 16:23:43 2023 +1100

    New version 0.4.0.1

[33mcommit 293efd3e7b0260be0746bc3c10d095ed8d8b0efb[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon Mar 6 16:23:24 2023 +1100

    Fix retry function

[33mcommit df00d9b7305185e55da3a89d45865476ded08e17[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon Mar 6 06:53:51 2023 +1100

    New version 0.4.0.0

[33mcommit 239bc0a5bf4fd523b41c9afb4e7242773dbd4a06[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon Mar 6 06:52:55 2023 +1100

    Pass retry count to retry function

[33mcommit 86d31672e4d01cc499b5b53adc34663ed41e0232[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon Mar 6 06:29:55 2023 +1100

    Pass retry count to retry function

[33mcommit 9ea1845ca2036e7a1ae9fcebe393526540c5c233[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon Jan 9 15:30:28 2023 +1100

    New version 0.3.0.3

[33mcommit a440f08eb0e4b7291ab9a0bad97a3bf73e3104a9[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon Jan 9 15:20:53 2023 +1100

    Fix moduleWorkspace

[33mcommit d6cdb7e875d749eb3080861c5e748dbd22d5e393[m[33m ([m[1;33mtag: v0.3.0.2[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Sat Jan 7 18:06:19 2023 +1100

    New version 0.3.0.2

[33mcommit c3469d01ba7bcf27083edd13dde81f07f7d0fe2b[m
Author: John Ky <john.ky@iohk.io>
Date:   Sat Jan 7 18:05:56 2023 +1100

    Generic instance

[33mcommit 439c68f57117eac219a71e57a2fcb1a33a09bbf3[m[33m ([m[1;33mtag: v0.3.0.1[m[33m)[m
Author: John Ky <john.ky@iohk.io>
Date:   Sat Jan 7 11:48:44 2023 +1100

    New version 0.3.0.1

[33mcommit 5ae711f0344d3237e824961be4b9fcf5201cce3a[m
Author: Markus Läll <markus.l2ll@gmail.com>
Date:   Thu Dec 15 13:22:48 2022 +0200

    Don't create an intermediate workspace directory

[33mcommit 91b4d0b38849f7a4b69d24cb7d748838f8b17c8d[m
Author: John Ky <john.ky@iohk.io>
Date:   Fri Jan 6 16:21:06 2023 +1100

    Find the nearest plan.json

[33mcommit 4135b9f7f1af31c7c24bf87ce708b99e41001296[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Jan 3 07:39:57 2023 +1100

    Restructure cabal file

[33mcommit afd5a50fafaa98b91d9dadf16f086ab135af1daa[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Jan 3 07:32:06 2023 +1100

    Update copyright

[33mcommit 21f53ab9225cc09fbd1eab341fe32e58b2c8e8bd[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Jan 3 06:44:16 2023 +1100

    Copy over waitNamedPipe from Win32-network

[33mcommit 79346a34b3e41e59988e346e9b90d84ce45f435e[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Jan 3 05:22:56 2023 +1100

    Remove unused dependency on Win32-network

[33mcommit 93be00a17ba3206f4adec7b0633adb3083ae6f28[m
Author: John Ky <newhoggy@gmail.com>
Date:   Mon Jan 2 12:54:36 2023 +1100

    Use setup-haskell action

[33mcommit 61da3e1a833d7eb5b882a0da49b594a0cc368d96[m
Author: John Ky <newhoggy@gmail.com>
Date:   Tue Dec 13 13:26:32 2022 +1100

    New version 0.3.0.0

[33mcommit 0397e0a890173ee4d990dc7d76b48e9ffbebd9c2[m
Author: John Ky <newhoggy@gmail.com>
Date:   Mon Dec 12 11:41:30 2022 +1100

    Add errorMessage to argument of deadline functions

[33mcommit 26b76bbcecfe48d0a87099f213f58e3778aa1f59[m
Author: Jean-Baptiste Giraudeau <jb@giraudeau.info>
Date:   Sat Sep 3 12:10:10 2022 +1000

    getProjectBase: explore up the dir hierarchy to find cabal.project

[33mcommit 113f77bf071f6304cb888da407c99c324fc69a76[m
Author: Tim McGilchrist <timmcgil@gmail.com>
Date:   Sat Aug 20 13:38:08 2022 +1000

    Update GH actions versions

[33mcommit 714ee03a5a786a05fc57ac5d2f1c2edce4660d85[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Jun 14 20:46:12 2022 +1000

    New fromJustM function

[33mcommit 881f15055627e321d29fa3bc63cfe60489a12675[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Jun 7 13:30:45 2022 +1000

    Add .gitignore file

[33mcommit 928458b14f8632e0aaebf19cccd4a0d04531e171[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed Apr 13 00:10:16 2022 +1000

    Relax bounds on aeson

[33mcommit f761c2532f5a3e2c98d2c9f51ad7d4c6c1f4f8e4[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Apr 12 22:53:05 2022 +1000

    New rewriteArrayElements function

[33mcommit 967d79533c21e33387d0227a5f6cc185203fe658[m
Author: Nicholas Clarke <nick@topos.org.uk>
Date:   Mon Mar 14 13:14:33 2022 +0100

    Update to support aeson >= 2.0.0 (#8)
    
    Bump the version number here accordingly. `rewriteObject` could now
    probably take a nicer interface, but we retain the old one for
    compatiblity.

[33mcommit 4190fe64c9269ff10cb7cb363b82c521b8e1093b[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Feb 15 01:00:16 2022 +1100

    Replace waitByDeadlineX functions to byDeadlineX functions that trigger retries by assertion failure. (#7)
    
    New headM function that is like head, but also asserts list is non-empty.
    New byDurationX functions for those cases where it is more convenient.

[33mcommit d9b96b58e9216b14a831830b2ddc99644f805d73[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon Jan 31 20:53:42 2022 +1100

    Add retry support (#6)

[33mcommit edf6945007177a638fbeb8802397f3a6f4e47c14[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Jul 13 11:01:14 2021 +1000

    Module re-exports (#5)

[33mcommit f7adf144bbd6da1aae9a668f40a1f8fdf69c9135[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed Jul 7 15:43:21 2021 +1000

    Generic error handling functions (#4)
    
    * Generic error handling functions
    
    * Use Last instead of Maybe in EnvConfig

[33mcommit 2f28e62f1508f07bb628963ee9bb23dc19ec0e03[m
Author: John Ky <john.ky@iohk.io>
Date:   Fri Jun 25 18:53:06 2021 +1000

    New exec, binFlex functions.  New execConfigCwd field (#3)

[33mcommit ccb5648ad3c93744f67e244706a7a3e36dcfbab0[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Jun 15 16:53:53 2021 +1000

    Add support for YAML (#2)

[33mcommit 8605fe46ee3f7d391f5dc6387a2245b4beada407[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Jun 15 16:42:58 2021 +1000

    Force evaluation of string for cat (#1)

[33mcommit 8bcd3c9dc22cc44f9fcfe161f4638a384fc7a187[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon Mar 29 16:54:22 2021 +1100

    Downgrade cabal file version to 2.4 because 3.0 is incompatible with stack

[33mcommit bee1b3018bef80384cc38b068289fc2efd065509[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon Mar 29 16:29:21 2021 +1100

    Setup Github Actions CI

[33mcommit 9bc9c869d0ac66166434bdb1235399017c00c66c[m
Author: John Ky <john.ky@iohk.io>
Date:   Fri Mar 26 10:49:00 2021 +1100

    Harmonise component versions and remove redundant conditionals.

[33mcommit 38c3179a4d71bc609c2d3756997a8a3628cee494[m
Author: John Ky <john.ky@iohk.io>
Date:   Thu Mar 18 15:45:39 2021 +1100

    Drop support for ghc-8.6.5

[33mcommit 7cfe42d450d5a518c547aa72808e4c038e8c1280[m
Author: John Ky <john.ky@iohk.io>
Date:   Thu Mar 11 23:02:38 2021 +1100

    Import cardano-submit-api from cardano-rest repo
    Publish submit-api artifacts
    Tidy up imports
    Remove unused cardano-cli dependencies
    Document how to test

[33mcommit b8bc70321f440719e9401898fbbae4635c63d1d7[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed Mar 10 16:34:41 2021 +1100

    Test support for running CLI commands with environment variables.

[33mcommit 4793a12db277a3f4b40aeffb6215ec2c5583ce9f[m
Author: John Ky <john.ky@iohk.io>
Date:   Fri Mar 5 18:29:44 2021 +1100

    Run golden tests at the value level rather than the text level to avoid chaing parser implementations causing tests to fail.

[33mcommit f06343e6b004a26c4aecd1e0e03b1f5d8ce02aa3[m
Author: Jordan Millar <jordan.millar@iohk.io>
Date:   Fri Dec 18 14:20:07 2020 +0000

    Update copyright to 2021

[33mcommit e304e1922fec64e33a01082b38dacb83d3b1e9c9[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed Dec 23 22:24:19 2020 +1100

    Cabal 3.4 in Github Actions

[33mcommit 08a4182fd632f7a737320a37a8ab0e282c58be5f[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Nov 10 15:37:01 2020 +1100

    Reconfigure nodes for chairman tests and tweak chairman block expectations.
    
    This includes:
    
    * slotDuration=2000
    * k=10
    * epochLength=1000
    * slotLength=0.2
    * systemStart=15 or 31 dependening on era
    * PBftSignatureThreshold=0.6
    * TraceChainDb=True
    
    Allow for the different testnets to have different expected number of blocks produced.
    
    Add DISABLE_CHAIRMAN environment variable for disabling the chairman check.
    
    Add KEEP_WORKSPACE environment variable for keeping the generated test workspaces even if tests succeed.
    
    New waitByDeadlineM and waitByDeadlineIO functions

[33mcommit 5d2aedaa273ccf9bfaa14a1030dcfd22258a2574[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon Nov 9 16:34:07 2020 +1100

    New copyRewriteJsonFile function to work around permissioning issues on Hydra

[33mcommit 85faabc395ca80446e4de2ade0f605894d7c2a80[m
Author: Thomas Winant <thomas@well-typed.com>
Date:   Mon Oct 19 08:05:26 2020 +0200

    Remove redundant imports
    
    GHC 8.10 detects more redundant warnings than previous versions. Removing
    them *doesn't* break compatibility with GHC 8.6, they were already redundant
    imports for GHC 8.6, the compiler just didn't realise it before.

[33mcommit 5ea24dcbfddbd127e419c3b36de46c56221095a1[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed Oct 14 15:38:13 2020 +1100

    General chairman test code quality changes

[33mcommit e68359a4145d107314624b68505b6b8b07b62c2c[m
Author: John Ky <john.ky@iohk.io>
Date:   Thu Sep 24 15:22:57 2020 +1000

    Chairman test running on Windows and Linux.

[33mcommit 9cb3be7a53b242d366e290e1204621ac0824620c[m
Author: John Ky <john.ky@iohk.io>
Date:   Fri Oct 2 16:10:48 2020 +1000

    Report all log files on any test failure.

[33mcommit 750d428cb74ed0d92864c1507681a365ca74c761[m
Author: Duncan Coutts <duncan@well-typed.com>
Date:   Thu Oct 1 15:37:36 2020 +0100

    Bump versions to 1.21.0 and update the change logs

[33mcommit dcc8f1f8439459de031217f64eec023a0275172b[m
Author: John Ky <john.ky@iohk.io>
Date:   Tue Sep 29 13:31:01 2020 +1000

    Run byron-shelley testnet

[33mcommit de93e4d6faf51746c8c7836fc82d84e4fa055363[m
Author: John Ky <john.ky@iohk.io>
Date:   Mon Sep 28 14:23:08 2020 +1000

    Run a chairman process per node

[33mcommit 639c7b87344560014a0952aa40295340d8415934[m
Author: John Ky <john.ky@iohk.io>
Date:   Wed Sep 16 10:44:43 2020 +1000

    Increase test code re-use by moving test support code into separate common hedgehog-extras library.
    
    * Move generic test code into the new hedgehog-extras library with a module structure consistent with its genericity.
    * Both cardano-node-chairman and cardano-cli modified to use the new library
    * Avoid using System.IO libraries directly in cardano-cli because exceptions cause test annotations to show making tests difficult. Instead use equivalent functions from hedgehog-extras
    * Re-implement execCardanoCLI in terms of execFlex which has the advantage of not depending on cabal exec which occasionally causes problems in terms of pollution of stdout
    * New textReadFile and textWriteFile functions
