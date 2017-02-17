package org.eolang.compiler;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

final class EOResourceFiles {
    private final String path;

    public EOResourceFiles(String path) {
        this.path = path;
    }

    public String formattedNames() {
        try {
            final List<String> filenames = new ArrayList<>(0);
            try (
                BufferedReader br =
                    new BufferedReader(
                        new InputStreamReader(
                            new Resource(path).asStream()))) {
                String resource;
                while ((resource = br.readLine()) != null) {
                    if ("eo".equals(FilenameUtils.getExtension(resource))) {
                        filenames.add(resource);
                    }
                }
            }
            return String.join("\n ", filenames);
        } catch (final IOException ex) {
            return "Error reading resource file.";
        }
    }

    public String eo(String filename) {
        try {
            return "\nEOLANG:\n" +
                IOUtils.toString(
                    new Resource("eo\\" + filename).asStream(),
                    Charset.defaultCharset()
                );
        } catch (IOException e) {
            return "Error reading resource file.";
        }
    }

    public String java(String filename) {
        final StringBuilder sb = new StringBuilder(100);
        sb.append("\nJAVA:\n" );
        try {
            final Program program = new Program(
                IOUtils.toString(
                    new Resource("eo\\" + filename).asStream(),
                    Charset.defaultCharset()
                )
            );
            program.save(new StringOutput(sb));
            sb.append("\n" );
            return sb.toString();
        } catch (IOException e) {
            return "Error reading resource file.";
        }
    }
}
