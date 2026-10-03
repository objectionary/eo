/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.locks.ReentrantLock;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * A numbered subdirectory of {@code target/eo}.
 *
 * <p>No stage picks its own number any more. A name that already owns a
 * {@code NN-name} directory under {@code target} keeps that number, found
 * by reading the directory itself rather than by replaying how this build
 * reached it; a name with none yet is given the number past the highest one
 * already taken, and that empty directory is created on the spot so the
 * reservation is visible to whoever asks next, in this build or a later
 * one. A stage that this build never reaches because an earlier one was
 * cached therefore does not shift the numbers a later build gives to the
 * stages that do run, and the same {@code target} never grows two
 * directories for the same name.</p>
 *
 * @since 0.72.0
 */
final class Subdir {

    /**
     * The shape of an already-numbered subdirectory: its number and name.
     */
    private static final Pattern PREFIXED = Pattern.compile("(\\d+)-(.+)");

    /**
     * The number already found on disk, or given, for each name asked for
     * so far, per target directory.
     */
    private static final Map<Path, ConcurrentMap<String, Integer>> NUMBERED =
        new ConcurrentHashMap<>();

    /**
     * One lock per target directory, guarding the read-and-reserve of a
     * number so two names never claim the same one.
     */
    private static final Map<Path, ReentrantLock> LOCKS = new ConcurrentHashMap<>();

    /**
     * The {@code target/eo} directory this subdirectory lives under.
     */
    private final Path target;

    /**
     * The name of this subdirectory, without its numeric prefix.
     */
    private final String name;

    /**
     * Ctor.
     *
     * @param tgt The {@code target/eo} directory this subdirectory lives under
     * @param nme The name of this subdirectory, without its numeric prefix
     */
    Subdir(final File tgt, final String nme) {
        this(tgt.toPath(), nme);
    }

    /**
     * Ctor.
     *
     * @param tgt The {@code target/eo} directory this subdirectory lives under
     * @param nme The name of this subdirectory, without its numeric prefix
     */
    Subdir(final Path tgt, final String nme) {
        this.target = tgt;
        this.name = nme;
    }

    /**
     * The path of this subdirectory, unless a mojo parameter already
     * names one to use instead.
     *
     * @param configured The value of the parameter, or null when unset
     * @return The path to use
     */
    Path orConfigured(final File configured) {
        final Path path;
        if (configured == null) {
            path = this.path();
        } else {
            path = configured.toPath();
        }
        return path;
    }

    /**
     * The path of this subdirectory as the disk already has it, unless a
     * mojo parameter already names one to use instead.
     *
     * @param configured The value of the parameter, absent when unset
     * @return The path to read
     */
    Path foundOrConfigured(final File configured) {
        return Optional.ofNullable(configured).map(File::toPath).orElseGet(this::found);
    }

    /**
     * The path of this subdirectory as the disk already has it.
     *
     * <p>Nothing is created and no number is reserved, unlike
     * {@link #path()}, so a goal that only reads a stage leaves no empty
     * directory behind. A stage with no directory yet gets its unnumbered
     * path under the target, which no stage ever occupies, so that the
     * caller finds it absent and says so.</p>
     *
     * @return The path, which is no directory when the stage never ran
     */
    Path found() {
        return this.owned().orElseGet(() -> this.target.resolve(this.name));
    }

    /**
     * The path of this subdirectory.
     *
     * @return The path
     */
    Path path() {
        return this.target.resolve(String.format("%02d-%s", this.number(), this.name));
    }

    private Optional<Path> owned() {
        final Optional<Path> found;
        if (Files.isDirectory(this.target)) {
            try (Stream<Path> kids = Files.list(this.target)) {
                found = kids
                    .filter(Files::isDirectory)
                    .filter(kid -> this.owns(kid.getFileName().toString()))
                    .findFirst();
            } catch (final IOException ex) {
                throw new UncheckedIOException(
                    String.format(
                        "Failed to look for '%s' under %s", this.name, this.target
                    ),
                    ex
                );
            }
        } else {
            found = Optional.empty();
        }
        return found;
    }

    private boolean owns(final String dir) {
        final Matcher matcher = Subdir.PREFIXED.matcher(dir);
        return matcher.matches() && matcher.group(2).equals(this.name);
    }

    private int number() {
        return Subdir.NUMBERED
            .computeIfAbsent(this.target, ignored -> new ConcurrentHashMap<>())
            .computeIfAbsent(this.name, ignored -> this.reserved());
    }

    private int reserved() {
        final ReentrantLock lock = Subdir.LOCKS.computeIfAbsent(
            this.target, ignored -> new ReentrantLock()
        );
        lock.lock();
        try {
            Files.createDirectories(this.target);
            final List<Matcher> taken;
            try (Stream<Path> kids = Files.list(this.target)) {
                taken = kids
                    .filter(Files::isDirectory)
                    .map(kid -> Subdir.PREFIXED.matcher(kid.getFileName().toString()))
                    .filter(Matcher::matches)
                    .collect(Collectors.toList());
            }
            final Optional<Integer> owned = taken.stream()
                .filter(matcher -> matcher.group(2).equals(this.name))
                .map(matcher -> Integer.parseInt(matcher.group(1)))
                .findFirst();
            final int number;
            if (owned.isPresent()) {
                number = owned.get();
            } else {
                number = this.claimed(
                    1 + taken.stream()
                        .mapToInt(matcher -> Integer.parseInt(matcher.group(1)))
                        .max()
                        .orElse(0)
                );
            }
            return number;
        } catch (final IOException ex) {
            throw new UncheckedIOException(
                String.format(
                    "Failed to number '%s' under %s", this.name, this.target
                ),
                ex
            );
        } finally {
            lock.unlock();
        }
    }

    private int claimed(final int number) throws IOException {
        final Path path = this.target.resolve(
            String.format("%02d-%s", number, this.name)
        );
        int result = number;
        try {
            Files.createDirectory(path);
        } catch (final FileAlreadyExistsException collision) {
            if (!Files.isDirectory(path)) {
                throw collision;
            }
            result = this.reserved();
        }
        return result;
    }
}
