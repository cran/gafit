gafit <- function( target, start, thermal=0.1, maxiter=50, samples=10, step=1e-3 )
{
    env <- new.env()
    for( j in names( start ))  # Put list names into environment
    {
        assign( j, start[ j ][[ 1 ]], envir=env )
    }
    gafit.workspace <- .Call( "gafit", target, env, thermal, maxiter, samples, step, PACKAGE="gafit" );
    start.copy <- start
    for( j in names( start.copy ))  # Pull the values back out of the environment
    {
	start.copy[ j ][[ 1 ]] <- eval( as.name( j ), envir=env )
    }
    attr( start.copy, "score" ) <- gafit.workspace[ 1 ]
    start.copy # Return the list containing our best guess
}
