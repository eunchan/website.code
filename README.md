# Static Website Generator for Eunchan.Kim

This [Hakyll][] configuration is for [Eunchan.Kim website](https://www.eunchan.kim).
It is based on hakyll example but a few features are added.

[Hakyll]: http://jaspervdj.org/hakyll

1.  Nightsky observation log is added.
    So, all the logs can be viewed on the Google Maps.
2.  Global link (markdown format) is used for entire site.
    `links.md` is loaded for every post.
    Also, if any post defines `slug` metadata, then it is used as link.
    This helps to keep the intra website link correctly.
3.  Yearly archives are added for normal pages (excluding Blogs and Nightsky observation logs).
    Archive links will be in the format of `archives/YYYY/index.html`
4.  Blogs has teaser list page as a front.
