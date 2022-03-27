# Run it using "bash gitarchive.sh <branch-name>"

if ! git tag archive/$1 $1
then
    echo "> tagging failed"
    exit 1
fi

if ! git branch -d $1
then
    echo "> branch deletion failed, would you like to hard delete $1? [y/N]"
    read response
    if [[ "$response" == "y" ]]
    then
        git branch -D newbranch
    else
        git tag -d archive/$1
        exit 1
    fi
fi

if ! git push origin :$1
then
    git tag -d archive/$1
    echo "> branch push failed"
    exit 1
fi

if ! git push --tags
then
    git tag -d archive/$1
    echo "> tag push failed"
fi