package org.eolang.parser;

public enum ErrorSeverity {
    ERROR("error"),
    WARNING("warning"),
    ADVICE("advice");

    private final String text;

    ErrorSeverity(final String text) {
        this.text = text;
    }

    public String asText() {
        return this.text;
    }
}
