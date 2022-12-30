package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Xsline;
import java.io.ByteArrayOutputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.eolang.maven.util.Home;
import org.eolang.parser.Syntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class JavaFilesTest {

    @Test
    void convertsXMLtoJavaSuccessfully(@TempDir final Path temp) throws Exception {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new Syntax(
            "sum.eo",
            new ResourceOf("org/eolang/maven/sum.eo"),
            new OutputTo(baos)
        ).parse();
        final String parsed = new Xsline(TranspileMojo.TRAIN).pass(
            new XMLDocument(baos.toByteArray())).toString();
        new Home(temp.resolve("xml")).save(parsed, Paths.get("sum.xmir"));
        final List<Path> files = new JavaFiles(temp.resolve("xml").resolve("sum.xmir"),
            temp.resolve("java")
        ).save();
        MatcherAssert.assertThat(
            new TextOf(new InputOf(files.get(0))).asString(),
            Matchers.containsString("final class EOsum extends PhDefault")
        );
    }

    @Test
    void convertsXMLtoJavaWithoutJavaClasses(@TempDir final Path temp) throws Exception {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new Syntax(
            "sum.eo",
            new ResourceOf("org/eolang/maven/sum.eo"),
            new OutputTo(baos)
        ).parse();
        final String parsed = new XMLDocument(baos.toByteArray()).toString();
        new Home(temp.resolve("xml")).save(parsed, Paths.get("sum.xmir"));
        MatcherAssert.assertThat(
            new JavaFiles(
                temp.resolve("xml").resolve("sum.xmir"),
                temp.resolve("java")
            ).save(), Matchers.empty()
        );
    }
}