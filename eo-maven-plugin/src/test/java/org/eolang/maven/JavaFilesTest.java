package org.eolang.maven;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class JavaFilesTest {

    @Test
    void convertsXMLtoJava(@TempDir final Path temp) throws IOException {
        final Path java = temp.resolve("java");
        new JavaFiles(temp.resolve("xml"), java).save();
        final File file = java.toFile();
        MatcherAssert.assertThat(file, FileMatchers.anExistingDirectory());
        MatcherAssert.assertThat(file.listFiles(), Matchers.not(Matchers.emptyArray()));
    }

}