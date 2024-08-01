/*
 * Copyright 2008-2013 Don Brown
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.eolang.maven;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.configuration.xml.XmlPlexusConfiguration;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.aether.RepositorySystemSession;
import org.hamcrest.CoreMatchers;
import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.TypeSafeDiagnosingMatcher;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.AdditionalMatchers;
import org.mockito.Mock;

import java.util.List;
import java.util.Map;
import org.mockito.junit.MockitoJUnitRunner;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.hamcrest.MockitoHamcrest.argThat;
import static org.twdata.maven.mojoexecutor.MojoExecutor.artifactId;
import static org.twdata.maven.mojoexecutor.MojoExecutor.attribute;
import static org.twdata.maven.mojoexecutor.MojoExecutor.attributes;
import static org.twdata.maven.mojoexecutor.MojoExecutor.configuration;
import static org.twdata.maven.mojoexecutor.MojoExecutor.dependencies;
import static org.twdata.maven.mojoexecutor.MojoExecutor.dependency;
import static org.twdata.maven.mojoexecutor.MojoExecutor.element;
import static org.twdata.maven.mojoexecutor.MojoExecutor.executeMojo;
import static org.twdata.maven.mojoexecutor.MojoExecutor.executionEnvironment;
import static org.twdata.maven.mojoexecutor.MojoExecutor.goal;
import static org.twdata.maven.mojoexecutor.MojoExecutor.groupId;
import static org.twdata.maven.mojoexecutor.MojoExecutor.name;
import static org.twdata.maven.mojoexecutor.MojoExecutor.plugin;
import static org.twdata.maven.mojoexecutor.MojoExecutor.version;

@RunWith(MockitoJUnitRunner.class)
public class MojoExecutorTest {
    @Mock MavenProject project;
    @Mock MavenSession session;
    @Mock RepositorySystemSession repositorySession;
    @Mock BuildPluginManager pluginManager;

    private MojoDescriptor copyDependenciesMojoDescriptor;

    @Before
    public void setUpMocks() throws Exception {
        PluginDescriptor mavenDependencyPluginDescriptor = new PluginDescriptor();

        copyDependenciesMojoDescriptor = new MojoDescriptor();
        copyDependenciesMojoDescriptor.setGoal("copy-dependencies");
        copyDependenciesMojoDescriptor.setConfiguration(new XmlPlexusConfiguration("configuration"));
        copyDependenciesMojoDescriptor.setPluginDescriptor(mavenDependencyPluginDescriptor);

        mavenDependencyPluginDescriptor.addMojo(copyDependenciesMojoDescriptor);

        when(session.getRepositorySession()).thenReturn(repositorySession);
        when(pluginManager.loadPlugin(
            eq(plugin(
                groupId("org.apache.maven.plugins"),
                artifactId("maven-dependency-plugin"),
                version("2.0"),
                dependencies(
                    dependency("org.apache.maven.plugins", "some-plugin", "1.0")
                )
            )),
            AdditionalMatchers.or(anyList(), isNull()),
            same(repositorySession)
        )).thenReturn(mavenDependencyPluginDescriptor);
    }

    @Test
    public void executeMojoWithoutExecutionIdExecutesMojoWithAttributesInConfiguration() throws Exception {
        executeMojo(
            plugin(
                groupId("org.apache.maven.plugins"),
                artifactId("maven-dependency-plugin"),
                version("2.0"),
                dependencies(
                    dependency("org.apache.maven.plugins", "some-plugin", "1.0")
                )
            ),
            goal("copy-dependencies"),
            configuration(
                element(
                    name("outputDirectory"),
                    attributes(
                        attribute("dir", "${project.build.directory}/foo"),
                        attribute("force", "true")
                    )
                ),
                element(
                    "without-attrs",
                    element(
                        "without-attrs", "value"
                    )
                ),
                element(
                    "only-attribute",
                    attribute("attribute", "value")
                )
            ),
            executionEnvironment(
                project,
                session,
                pluginManager
            )
        );
        verify(pluginManager)
            .executeMojo(
                same(session),
                argThat(is(equalTo(new MojoExecution(
                    copyDependenciesMojoDescriptor,
                    configuration(
                        element(
                            name("outputDirectory"),
                            attributes(
                                attribute("dir", "${project.build.directory}/foo"),
                                attribute("force", "true")
                            )
                        ),
                        element(
                            "without-attrs",
                            element(
                                "without-attrs", "value"
                            )
                        ),
                        element(
                            "only-attribute",
                            attributes(
                                attribute("attribute", "value")
                            )
                        )
                    )
                ))))
            );
    }

    @Test
    public void executeMojoWithoutExecutionIdExecutesMojoWithExplicitConfiguration() throws Exception {

        executeMojo(
            plugin(
                groupId("org.apache.maven.plugins"),
                artifactId("maven-dependency-plugin"),
                version("2.0"),
                dependencies(
                    dependency("org.apache.maven.plugins", "some-plugin", "1.0")
                )
            ),
            goal("copy-dependencies"),
            configuration(
                element(name("outputDirectory"), "${project.build.directory}/foo")
            ),
            executionEnvironment(
                project,
                session,
                pluginManager
            )
        );
        verify(pluginManager)
            .executeMojo(
                same(session),
                argThat(is(equalTo(new MojoExecution(
                    copyDependenciesMojoDescriptor,
                    configuration(
                        element(name("outputDirectory"), "${project.build.directory}/foo")
                    )
                ))))
            );
    }

    @Test
    public void executeMojoWithoutExecutionIdExecutesMojoWithExplicitConfigurationNoProject() throws Exception {

        executeMojo(
            plugin(
                groupId("org.apache.maven.plugins"),
                artifactId("maven-dependency-plugin"),
                version("2.0"),
                dependencies(
                    dependency("org.apache.maven.plugins", "some-plugin", "1.0")
                )
            ),
            goal("copy-dependencies"),
            configuration(
                element(name("outputDirectory"), "${project.build.directory}/foo")
            ),
            executionEnvironment(
                null,
                session,
                pluginManager
            )
        );
        verify(pluginManager)
            .executeMojo(
                same(session),
                argThat(is(equalTo(new MojoExecution(
                    copyDependenciesMojoDescriptor,
                    configuration(
                        element(name("outputDirectory"), "${project.build.directory}/foo")
                    )
                ))))
            );
    }

    @Test
    public void executeMojoWithoutExecutionIdExecutesMojoWithExplicitConfigurationIgnoreProject() throws Exception {

        executeMojo(
            plugin(
                groupId("org.apache.maven.plugins"),
                artifactId("maven-dependency-plugin"),
                version("2.0"),
                dependencies(
                    dependency("org.apache.maven.plugins", "some-plugin", "1.0")
                )
            ),
            goal("copy-dependencies"),
            configuration(
                element(name("outputDirectory"), "${project.build.directory}/foo")
            ),
            executionEnvironment(
                session,
                pluginManager
            )
        );
        verify(pluginManager)
            .executeMojo(
                same(session),
                argThat(is(equalTo(new MojoExecution(
                    copyDependenciesMojoDescriptor,
                    configuration(
                        element(name("outputDirectory"), "${project.build.directory}/foo")
                    )
                ))))
            );
    }

    @Test
    public void executeMojoWithExecutionIdExecutesMojoWithMatchingExecutionId() throws Exception {

        executeMojo(
            plugin(
                groupId("org.apache.maven.plugins"),
                artifactId("maven-dependency-plugin"),
                version("2.0"),
                dependencies(
                    dependency("org.apache.maven.plugins", "fake-plugin", "1.0")
                )
            ),
            goal("copy-dependencies#execution"),
            configuration(
                element(name("outputDirectory"), "${project.build.directory}/foo")
            ),
            executionEnvironment(
                project,
                session,
                pluginManager
            )
        );
        verify(pluginManager)
            .executeMojo(
                same(session),
                argThat(is(equalTo(new MojoExecution(copyDependenciesMojoDescriptor, "execution"))))
            );
    }

    private static Matcher<MojoExecution> equalTo(MojoExecution mojoExecution) {
        return new MojoExecutionIsEqual(mojoExecution);
    }

    // This is needed because the equalTo(MojoExecution method above shadows a static import of CoreMatchers.equalTo.
    private static <T> Matcher<? super T> equalTo(T match) {
        return CoreMatchers.equalTo(match);
    }

    private static class MojoExecutionIsEqual extends TypeSafeDiagnosingMatcher<MojoExecution> {
        private final Matcher<? super Plugin> plugin;
        private final Matcher<? super String> goal;
        private final Matcher<? super String> executionId;
        private final Matcher<? super MojoDescriptor> mojoDescriptor;
        private final Matcher<? super Xpp3Dom> configuration;
        private final Matcher<? super MojoExecution.Source> source;
        private final Matcher<? super String> lifecyclePhase;
        private final Matcher<? super Map<String, List<MojoExecution>>> forkedExecutions;

        MojoExecutionIsEqual(MojoExecution mojoExecution) {
            plugin = is(equalTo(mojoExecution.getPlugin()));
            goal = is(equalTo(mojoExecution.getGoal()));
            executionId = is(equalTo(mojoExecution.getExecutionId()));
            mojoDescriptor = is(equalTo(mojoExecution.getMojoDescriptor()));
            configuration = is(equalTo(mojoExecution.getConfiguration()));
            source = is(sameInstance(mojoExecution.getSource()));
            lifecyclePhase = is(equalTo(mojoExecution.getLifecyclePhase()));
            forkedExecutions = is(equalTo(mojoExecution.getForkedExecutions()));
        }

        @Override
        protected boolean matchesSafely(MojoExecution mojoExecution, Description mismatchDescription) {
            boolean matches = true;
            matches = tryMatch("plugin", plugin, mojoExecution.getPlugin(), mismatchDescription, matches);
            matches = tryMatch("goal", goal, mojoExecution.getGoal(), mismatchDescription, matches);
            matches = tryMatch("executionId", executionId, mojoExecution.getExecutionId(), mismatchDescription, matches);
            matches = tryMatch("mojoDescriptor", mojoDescriptor, mojoExecution.getMojoDescriptor(), mismatchDescription,
                matches);
            matches = tryMatch("configuration", configuration, mojoExecution.getConfiguration(), mismatchDescription,
                matches);
            matches = tryMatch("source", source, mojoExecution.getSource(), mismatchDescription, matches);
            matches = tryMatch("lifecyclePhase", lifecyclePhase, mojoExecution.getLifecyclePhase(), mismatchDescription,
                matches);
            matches = tryMatch("forkedExecutions", forkedExecutions, mojoExecution.getForkedExecutions(),
                mismatchDescription, matches);
            return matches;
        }

        private boolean tryMatch(String name, Matcher<?> matcher, Object item, Description mismatchDescription,
                                 boolean matches) {
            if (!matcher.matches(item)) {
                reportMismatch(name, matcher, item, mismatchDescription, matches);
                return false;
            }
            return matches;
        }

        private void reportMismatch(String name, Matcher<?> matcher, Object item, Description mismatchDescription,
                                    boolean firstMismatch) {
            if (!firstMismatch) mismatchDescription.appendText(", ");
            mismatchDescription.appendText(name).appendText(" ");
            matcher.describeMismatch(item, mismatchDescription);
        }

        public void describeTo(Description description) {
            description.appendText("MojoExecution with plugin ")
                .appendDescriptionOf(plugin)
                .appendText(", goal ")
                .appendDescriptionOf(goal)
                .appendText(", executionId ")
                .appendDescriptionOf(executionId)
                .appendText(", mojoDescriptor ")
                .appendDescriptionOf(mojoDescriptor)
                .appendText(", configuration ")
                .appendDescriptionOf(configuration)
                .appendText(", source ")
                .appendDescriptionOf(source)
                .appendText(", lifecyclePhase")
                .appendDescriptionOf(lifecyclePhase)
                .appendText(", forkedExecutions ")
                .appendDescriptionOf(forkedExecutions);
        }
    }
}