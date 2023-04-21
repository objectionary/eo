package org.eolang.maven.rust_project;

import com.sun.tools.javac.util.Pair;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.ConcurrentHashMap;
public class Names {

    /**
     * Cache directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    private final Path dest;

    private final ConcurrentHashMap<String, String> all;

    public Names(final Path cache) throws IOException {
        this.dest = cache.resolve("binarize");
        if (this.dest.resolve("names.txt").toFile().exists()) {
            try {
                this.all = (ConcurrentHashMap<String, String>) (new ObjectInputStream(new FileInputStream(cache.resolve("names.txt").toFile()))).readObject();
            } catch (ClassNotFoundException exc) {
                throw new IllegalArgumentException(
                    String.format(
                        "File %s contains invalid data",
                        this.dest
                    ),
                    exc
                );
            }
        }
        else {
            this.all = new ConcurrentHashMap<>();
        }
    }

    public String name(final String code, final String dependencies) {
        return this.all.computeIfAbsent(
            new Pair<>(code, dependencies).toString(),
            (key) -> String.format(
                "f%d",
                this.all.size()
            )
        );
    }

    public void save() throws IOException {
        Files.createDirectories(this.dest);
        new ObjectOutputStream(new FileOutputStream(this.dest.resolve("name.txt").toFile())).writeObject(this.all);
    }
}
