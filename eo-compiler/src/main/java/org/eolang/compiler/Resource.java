package org.eolang.compiler;

import java.io.IOException;
import java.io.InputStream;

class Resource {
    private final String resource;

    public Resource(final String resource) {
        this.resource = resource;
    }

    public InputStream asStream() throws IOException {
        InputStream input =
            Thread.currentThread().getContextClassLoader().getResourceAsStream(resource);
        if (input == null) {
            input = this.getClass().getResourceAsStream(resource);
        }
        if (input == null) {
            throw new IOException();
        }
        return input;
    }

}
