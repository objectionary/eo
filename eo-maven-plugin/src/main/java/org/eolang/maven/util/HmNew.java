package org.eolang.maven.util;

import org.cactoos.Bytes;
import org.cactoos.Input;
import org.cactoos.Text;
import org.cactoos.io.InputOf;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;

public class HmNew implements Home {
    private final BiIoProc<Input, Path> sve;

    HmNew(BiIoProc<Input, Path> save) {
        this.sve = save;
    }

    @Override
    public void save(String str, Path path) throws IOException {
        this.save(new InputOf(str), path);
    }

    @Override
    public void save(Text txt, Path path) throws IOException {
        this.save(new InputOf(txt), path);
    }

    @Override
    public void save(InputStream stream, Path path) throws IOException {
        this.save(new InputOf(stream), path);
    }

    @Override
    public void save(byte[] bytes, Path path) throws IOException {
        this.save(new InputOf(bytes), path);
    }

    @Override
    public void save(Input input, Path path) throws IOException {
        this.sve.exec(input, path);
    }

    @Override
    public boolean exists(Path path) {
        throw new IllegalStateException("Should never happen");
    }

    @Override
    public Bytes load(Path path) throws IOException {
        throw new IllegalStateException("Should never happen");
    }

    @Override
    public Path absolute(Path path) {
        throw new IllegalStateException("Should never happen");
    }

    @Override
    public Path onlyRelative(Path path) {
        throw new IllegalStateException("Should never happen");
    }
}
