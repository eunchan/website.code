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

## Templates

To build this website, couple of templates are required.

-   `default.html`: Main template. Contains head, and footer.
-   `disqus.html`: If metadata `disqus` field is true, this template will be rendered.
-   `teaser.html`: This template is for blog main page ('Journal').
-   `blogpage.html`: Since blog has previous post and next post, this template is to support the feature.
-   `post-list.html`: This is for list of posts that is included in index page and also list of sky logs.
-   `sites.json`: This template is for Nightsky sites. Nightsky page parses json then display it on Google Maps.
