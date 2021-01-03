#!/usr/bin/env python3
# File name: build.py
# Description: hts-project master build script
import sys
import os
import shutil
from distutils.dir_util import copy_tree


def print_default_msg():
    print(
        '''
        Please invoke script with one of the following arguments:
        build      - Builds project
        run        - Builds and then runs project
        clean      - Cleans project via stack
        deep-clean - Cleans project via git
        doc        - Cleans previously generated Haddock docs and re-generates
        test       - Runs test suite
        '''
    )


def build_project():
    os.system('stack build')


def run_project():
    os.system('stack run')


def clean_project():
    os.system('stack clean')


def deep_clean_project():
    # Show what will be deleted
    print("Command would do the following:")
    os.system('git clean -xdn')
    user_confirmed = input("Are you sure? Y/N: ")
    if user_confirmed == "Y":
        os.system('git clean -xdf')
    else:
        print("Deep clean cancelled...")


def wipe_dir(relative_dir):
    with os.scandir(relative_dir) as entries:
        for entry in entries:
            if entry.is_file() or entry.is_symlink():
                os.remove(entry.path)
            elif entry.is_dir():
                shutil.rmtree(entry.path)


# TODO: This function pretty much hard-coded the location of the Haddock docs
#       There must be a smarter way, to make this more portable...
def doc_project():
    # Run haddock through Cabal (using Stack seems buggy...)
    os.system('cabal haddock')

    # Clean the currently generated docs
    # Will then have to copy things out of dist -> into docs for convenience
    CABAL_HADDOCK_DIR = ".\\dist-newstyle\\build\\x86_64-windows\\ghc-8.10.2\\hts-project-0.1.0.0\\doc\\html\\hts-project\\"
    DOCS_DIR = ".\\docs\\"

    wipe_dir(DOCS_DIR)
    copy_tree(CABAL_HADDOCK_DIR, DOCS_DIR)


def test_project():
    os.system('stack test')


if __name__ == '__main__':
    args_num = len(sys.argv)

    if args_num == 1:
        print_default_msg()
    else:
        flag = str(sys.argv[1])
        if flag == "build":
            # Stack build
            build_project()
        elif flag == "run":
            # Stack run
            run_project()
        elif flag == "clean":
            # Stack clean
            clean_project()
        elif flag == "deep-clean":
            # Git clean
            deep_clean_project()
        elif flag == "doc":
            # Cabal Haddock
            doc_project()
        elif flag == "test":
            # Stack test
            test_project()
        else:
            print_default_msg()


