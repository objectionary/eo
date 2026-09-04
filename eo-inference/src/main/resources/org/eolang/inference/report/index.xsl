<?xml version="1.0" encoding="UTF-8"?>
<!--
SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="index" version="2.0">
  <xsl:output method="html" version="5.0" encoding="UTF-8" indent="no" omit-xml-declaration="yes"/>
  <xsl:template match="/index">
    <html lang="en">
      <head>
        <meta charset="utf-8"/>
        <title>what every object was copied from</title>
        <xsl:call-template name="style"/>
      </head>
      <body>
        <h1>what every object was copied from</h1>
        <p class="tally">
          <span class="named">
            <xsl:value-of select="@named"/>
            <xsl:text> named</xsl:text>
          </span>
          <span class="rooted">
            <xsl:value-of select="@rooted"/>
            <xsl:text> rooted at a void</xsl:text>
          </span>
          <span class="atom">
            <xsl:value-of select="@atom"/>
            <xsl:text> filled by an atom</xsl:text>
          </span>
          <span class="blank">
            <xsl:value-of select="@blank"/>
            <xsl:text> nothing known</xsl:text>
          </span>
        </p>
        <p class="note">A mark is green where we can name the formation an object was copied from, amber where all we have is a name rooted in a void the callers fill, violet where the void is one only an atom fills and no caller can be looked at, and red where we have nothing. Hover a mark to see what we found.</p>
        <xsl:apply-templates select="dir|file"/>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="dir">
    <details>
      <summary>
        <xsl:value-of select="@name"/>
        <xsl:call-template name="bar"/>
      </summary>
      <div class="in">
        <xsl:apply-templates select="dir|file"/>
      </div>
    </details>
  </xsl:template>
  <xsl:template match="file">
    <div class="row">
      <a href="{@href}">
        <xsl:value-of select="@name"/>
      </a>
      <xsl:call-template name="bar"/>
    </div>
  </xsl:template>
  <xsl:template name="bar">
    <span class="bar" title="{@named} named, {@rooted} rooted at a void, {@atom} filled by an atom, {@blank} nothing known">
      <span class="named" style="flex: {@named}"/>
      <span class="rooted" style="flex: {@rooted}"/>
      <span class="atom" style="flex: {@atom}"/>
      <span class="blank" style="flex: {@blank}"/>
    </span>
  </xsl:template>
  <xsl:template name="style">
    <style>
      body { font: 13px/1.6 ui-monospace, Menlo, Consolas, monospace; margin: 2em; max-width: 54em; color: #1a1a1a; background: #fff; }
      h1 { font-size: 15px; font-weight: 600; margin: 0 0 .8em; }
      a { color: #0645ad; text-decoration: none; }
      a:hover { text-decoration: underline; }
      .note { color: #555; margin: 0 0 1.6em; font-family: system-ui, sans-serif; }
      .tally span { margin-right: 1.4em; padding: .1em .4em; border-radius: 3px; }
      .tally .named { background: #d7f0d7; }
      .tally .rooted { background: #fbeecc; }
      .tally .atom { background: #e6dcf5; }
      .tally .blank { background: #f7d4d4; }
      details { margin: 0; }
      summary { cursor: pointer; display: flex; align-items: center; gap: .8em; padding: .1em 0; font-weight: 600; }
      .in { margin-left: 1.4em; }
      .row { display: flex; align-items: center; gap: .8em; padding: .1em 0; }
      .row a { flex: none; }
      .bar { display: flex; flex: 1; height: 8px; min-width: 8em; border-radius: 2px; overflow: hidden; background: #eee; }
      .bar .named { background: #6bbf6b; }
      .bar .rooted { background: #e8b64c; }
      .bar .atom { background: #9b7fd4; }
      .bar .blank { background: #d76b6b; }
    </style>
  </xsl:template>
</xsl:stylesheet>
