var gulp = require("gulp");
var browserify = require("browserify");
var source = require('vinyl-source-stream');
var concatCss = require('gulp-concat-css');
var tsify = require("tsify");
var paths = {
    pages: [
      'src/*.html',
    ],
    css: [
      'node_modules/codemirror/lib/codemirror.css',
      'node_modules/codemirror/theme/*.css',
      'css/*.css'
    ]
};

gulp.task("copy-html", function () {
    return gulp.src(paths.pages)
        .pipe(gulp.dest("dist"));
});

gulp.task("bundle-css", function () {
    return gulp.src(paths.css)
        .pipe(concatCss("bundle.css"))
        .pipe(gulp.dest("dist"));
});

gulp.task("default", ["copy-html", "bundle-css"], function () {
    return browserify({
        basedir: '.',
        debug: true,
        entries: ['src/main.tsx'],
        cache: {},
        packageCache: {}
    })
    .plugin(tsify)
    .bundle()
    .pipe(source('app.js'))
    .pipe(gulp.dest("dist"));
});
