<?xml version="1.0" encoding="UTF-8"?>
<!--
* SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
* SPDX-License-Identifier: MIT
-->
<!--
The index of the inference report: a bar per file with its objects in the
colours of ten bands; a folder's bar adds up its files. A click on
a colour in the tally keeps only the files and folders with objects of the
chosen colours; the file pages share the choice and keep only the lines with
such marks. Under the tally, a glossary says what the words of the report
mean.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="index" version="3.0">
  <xsl:output method="html" version="5.0" encoding="UTF-8" indent="no" omit-xml-declaration="yes" include-content-type="no"/>
  <xsl:template match="/index">
    <html lang="en">
      <head>
        <meta charset="utf-8"/>
        <title>what every object was copied from</title>
        <xsl:call-template name="style"/>
      </head>
      <body>
        <h1>what every object was copied from</h1>
        <p class="tally" title="Click colours to show only their objects; click a chosen colour again to drop it">
          <span class="named" data-band="named">
            <xsl:value-of select="@named"/>
            <xsl:text> named</xsl:text>
          </span>
          <span class="gaps" data-band="gaps">
            <xsl:value-of select="@gaps"/>
            <xsl:text> one name, with gaps</xsl:text>
          </span>
          <span class="pair" data-band="pair">
            <xsl:value-of select="@pair"/>
            <xsl:text> two names</xsl:text>
          </span>
          <span class="mixed" data-band="mixed">
            <xsl:value-of select="@mixed"/>
            <xsl:text> two or more names, maybe with gaps</xsl:text>
          </span>
          <span class="lost" data-band="lost">
            <xsl:value-of select="@lost"/>
            <xsl:text> lost track</xsl:text>
          </span>
          <span class="atom" data-band="atom">
            <xsl:value-of select="@atom"/>
            <xsl:text> open void, filled by an atom</xsl:text>
          </span>
          <span class="unfilled" data-band="unfilled">
            <xsl:value-of select="@unfilled"/>
            <xsl:text> open void, filled by nobody</xsl:text>
          </span>
          <span class="error" data-band="error">
            <xsl:value-of select="@error"/>
            <xsl:text> only an error</xsl:text>
          </span>
          <span class="unreached" data-band="unreached">
            <xsl:value-of select="@unreached"/>
            <xsl:text> not reached</xsl:text>
          </span>
          <span class="fails" data-band="fails">
            <xsl:value-of select="@fails"/>
            <xsl:text> a read fails</xsl:text>
          </span>
        </p>
        <dl class="terms">
          <dt>copy</dt>
          <dd>The engine answers an object once in every copy of the object around it, so a mark says what the object is in how many copies.</dd>
          <dt>name</dt>
          <dd>The object an answer is copied from, like Φ.number. A formation with no name of its own counts as one name per shape: its voids and what its @ gives, then a line per attribute it binds, with the attribute's own voids and what it gives, like [∅] → Φ.string.</dd>
          <dt>→ and ↺</dt>
          <dd>In a shape, → is what an @ gives, and ↺ is the object the shape is of: [^] → Φ.bytes with a line [^ ∅] > read ↺ is an object of bytes whose read gives the object back.</dd>
          <dt>error</dt>
          <dd>The copy gives T.</dd>
          <dt>lost track</dt>
          <dd>The engine can't tell what the copy gives.</dd>
          <dt>open void</dt>
          <dd>A void that no copy fills. An atom fills it when the object it sits on was returned by an atom, since Java fills that object.</dd>
          <dt>gap</dt>
          <dd>An error, lost track, or an open void.</dd>
        </dl>
        <xsl:apply-templates select="dir|file"/>
        <xsl:call-template name="filter"/>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="dir">
    <details data-named="{@named}" data-gaps="{@gaps}" data-pair="{@pair}" data-mixed="{@mixed}" data-lost="{@lost}" data-atom="{@atom}" data-unfilled="{@unfilled}" data-error="{@error}" data-unreached="{@unreached}" data-fails="{@fails}">
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
    <div class="row" data-named="{@named}" data-gaps="{@gaps}" data-pair="{@pair}" data-mixed="{@mixed}" data-lost="{@lost}" data-atom="{@atom}" data-unfilled="{@unfilled}" data-error="{@error}" data-unreached="{@unreached}" data-fails="{@fails}">
      <a href="{@href}">
        <xsl:value-of select="@name"/>
      </a>
      <xsl:call-template name="bar"/>
    </div>
  </xsl:template>
  <xsl:template name="bar">
    <span class="bar" title="{@named} named, {@gaps} one name with gaps, {@pair} two names, {@mixed} two or more names maybe with gaps, {@lost} lost track, {@atom} filled by an atom, {@unfilled} filled by nobody, {@error} only an error, {@unreached} not reached, {@fails} a read fails">
      <span class="named" style="flex: {@named}"/>
      <span class="gaps" style="flex: {@gaps}"/>
      <span class="pair" style="flex: {@pair}"/>
      <span class="mixed" style="flex: {@mixed}"/>
      <span class="lost" style="flex: {@lost}"/>
      <span class="atom" style="flex: {@atom}"/>
      <span class="unfilled" style="flex: {@unfilled}"/>
      <span class="error" style="flex: {@error}"/>
      <span class="unreached" style="flex: {@unreached}"/>
      <span class="fails" style="flex: {@fails}"/>
    </span>
  </xsl:template>
  <xsl:template name="style">
    <style>
      body { font: 13px/1.6 ui-monospace, Menlo, Consolas, monospace; margin: 2em; max-width: 54em; color: #1a1a1a; background: #fff; }
      h1 { font-size: 15px; font-weight: 600; margin: 0 0 .8em; }
      a { color: #0645ad; text-decoration: none; }
      a:hover { text-decoration: underline; }
      .tally span { margin-right: 1.4em; padding: .1em .4em; border-radius: 3px; display: inline-block; margin-bottom: .3em; }
      .tally .named { background: #d7f0d7; }
      .tally .gaps { background: #eef3c2; }
      .tally .pair { background: #d3eff2; }
      .tally .mixed { background: #d9e5fb; }
      .tally .lost { background: #fbeecc; }
      .tally .atom { background: #e6dcf5; }
      .tally .unfilled { background: #dfe6e9; }
      .tally .error { background: #efe2dc; }
      .tally .unreached { background: #eeeeee; }
      .tally .fails { background: #f7d4d4; }
      .terms { display: grid; grid-template-columns: max-content 1fr; gap: .3em 1.2em; margin: 0 0 1.6em; color: #555; font-family: system-ui, sans-serif; }
      .terms dt { color: #1a1a1a; font-weight: 600; }
      .terms dd { margin: 0; }
      .tally span[data-band] { cursor: pointer; user-select: none; }
      .tally span.on { box-shadow: 0 0 0 2px #1a1a1a; }
      .row.hidden, details.hidden { display: none; }
      details { margin: 0; }
      summary { cursor: pointer; display: flex; align-items: center; gap: .8em; padding: .1em 0; font-weight: 600; }
      .in { margin-left: 1.4em; }
      .row { display: flex; align-items: center; gap: .8em; padding: .1em 0; }
      .row a { flex: none; }
      .bar { display: flex; flex: 1; height: 8px; min-width: 8em; border-radius: 2px; overflow: hidden; background: #eee; }
      .bar .named { background: #6bbf6b; }
      .bar .gaps { background: #c3cf3e; }
      .bar .pair { background: #3fb3c2; }
      .bar .mixed { background: #5b8def; }
      .bar .lost { background: #e8b64c; }
      .bar .atom { background: #9b7fd4; }
      .bar .unfilled { background: #90a4ae; }
      .bar .error { background: #a1887f; }
      .bar .unreached { background: #cfcfcf; }
      .bar .fails { background: #d76b6b; }
    </style>
  </xsl:template>
  <xsl:template name="filter">
    <script>
      (function () {
        var chips = Array.from(document.querySelectorAll('.tally span[data-band]'));
        var rows = Array.from(document.querySelectorAll('.row, details'));
        function shown() {
          var picked = chips.filter(function (chip) { return chip.classList.contains('on'); }).map(function (chip) { return chip.dataset.band; });
          rows.forEach(function (row) {
            var some = picked.some(function (band) { return Number(row.dataset[band]) > 0; });
            row.classList.toggle('hidden', picked.length > 0 ? !some : false);
          });
          try { localStorage.setItem('bands', picked.join(' ')); } catch (ignored) {}
        }
        var saved = '';
        try { saved = localStorage.getItem('bands') || ''; } catch (ignored) {}
        chips.forEach(function (chip) {
          chip.classList.toggle('on', saved.split(' ').indexOf(chip.dataset.band) !== -1);
          chip.addEventListener('click', function () { chip.classList.toggle('on'); shown(); });
        });
        shown();
      })();
    </script>
  </xsl:template>
</xsl:stylesheet>
