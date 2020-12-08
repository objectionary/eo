package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSL;
import java.nio.file.Path;
import java.util.List;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.cactoos.scalar.LengthOf;
import org.cactoos.scalar.Unchecked;
import org.eolang.compiler.Program;

/**
 * The spy to log all results.
 */
final class TargetSpy implements Program.Spy {
    /**
     * The dir.
     */
    private final Path dir;

    /**
     * Ctor.
     * @param target The path
     */
    TargetSpy(final Path target) {
        this.dir = target;
    }

    @Override
    public void push(final int index, final XSL xsl, final XML xml) {
        final List<String> names = new XMLDocument(
            xsl.toString()
        ).xpath("/*/@id");
        final String file;
        if (names.isEmpty()) {
            file = String.format("%d", index);
        } else {
            file = names.get(0).replaceAll("[^a-z0-9]", "-");
        }
        new Unchecked<>(
            new LengthOf(
                new TeeInput(
                    new InputOf(xml.toString()),
                    new OutputTo(
                        this.dir.resolve(
                            String.format("%02d-%s.xml", index, file)
                        )
                    )
                )
            )
        ).value();
    }
}
