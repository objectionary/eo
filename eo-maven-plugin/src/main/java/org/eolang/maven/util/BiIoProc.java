package org.eolang.maven.util;

import org.cactoos.BiProc;

import java.io.IOException;

interface BiIoProc<T, U> extends BiProc<T, U> {
    @Override
    void exec(T t, U u) throws IOException;
}
