package org.eolang.maven;

import java.nio.file.Path;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.twdata.maven.mojoexecutor.MojoExecutor;

public class MavenDependencyTree implements DependencyTree {

    /**
     * Maven project.
     */
    private final MavenProject project;

    /**
     * Maven session.
     */
    private final MavenSession session;

    /**
     * Maven plugin manager.
     */
    private final BuildPluginManager manager;

    public MavenDependencyTree(
        final MavenProject project,
        final MavenSession session,
        final BuildPluginManager manager
    ) {
        this.project = project;
        this.session = session;
        this.manager = manager;
    }

    @Override
    public Path save(final Dependency origin, final Path dir) {
        try {
            final Path path = dir.resolve(MavenDependencyTree.fileName(origin));
            MojoExecutor.executeMojo(
                MojoExecutor.plugin(
                    MojoExecutor.groupId("com.github.ferstl"),
                    MojoExecutor.artifactId("depgraph-maven-plugin"),
                    MojoExecutor.version("4.0.2")
                ),
                MojoExecutor.goal("for-artifact"),
                MojoExecutor.configuration(
                    MojoExecutor.element("groupId", origin.getGroupId()),
                    MojoExecutor.element("artifactId", origin.getArtifactId()),
                    MojoExecutor.element("version", origin.getVersion()),
                    MojoExecutor.element("outputDirectory", path.toString())
                ),
                MojoExecutor.executionEnvironment(
                    this.project,
                    this.session,
                    this.manager
                )
            );
            return path;
        } catch (MojoExecutionException e) {
            throw new IllegalStateException(e);
        }
    }

    private static String fileName(final Dependency dependency) {
        return String.format(dependency.getClassifier(), ".json");
    }
}
