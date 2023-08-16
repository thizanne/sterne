'use strict';

import fs from 'fs';
import gulp from 'gulp';
import named from 'vinyl-named';
import plugins from 'gulp-load-plugins';
import rimraf from 'rimraf';
import webpack2 from 'webpack';
import webpackStream from 'webpack-stream';
import yaml from 'js-yaml';
import yargs from 'yargs';

// Load all Gulp plugins into one variable
const $ = plugins();

// Check for --production flag
const PRODUCTION = !!(yargs.argv.production);

// Load settings from settings.yml
const { PATHS } = loadConfig();

function loadConfig() {
    let ymlFile = fs.readFileSync('config.gulp.yml', 'utf8');
    return yaml.load(ymlFile);
}

// Delete the "dist" folder
// This happens every time a clean build starts
function clean(done) {
    rimraf(PATHS.dist, done);
}

// Copy fonts to the fonts folder
function fonts() {
    return gulp.src(PATHS.fonts)
        .pipe(gulp.dest(PATHS.dist + '/fonts'));
}

const assets =
      gulp.parallel(
          fonts
      );

// Compile Sass into CSS

const sassCompiler = require('gulp-sass')(require('sass')); // Use dart sass

function sass() {
    return gulp.src(PATHS.sass_entries)
        .pipe(sassCompiler({includePaths: PATHS.sass_libs})
              .on('error', sassCompiler.logError))
        .pipe(gulp.dest(PATHS.dist + '/css'))
}

// Combine JavaScript into one file
// In production, the file is minified

let webpackConfig = {
    mode: (PRODUCTION ? 'production' : 'development'),
    module: {
        rules: [
            {
                test: /\.js$/,
                use: {
                    loader: 'babel-loader',
                    options: {
                        presets: [ "@babel/preset-env" ],
                        compact: false
                    }
                }
            }
        ]
    },
    devtool: !PRODUCTION && 'source-map'
}

function source_javascript() {
    return gulp.src(PATHS.entries)
        .pipe(named())
        .pipe($.sourcemaps.init())
        .pipe(webpackStream(webpackConfig, webpack2))
        .pipe($.if(PRODUCTION, $.uglify().on('error', e => { console.log(e); })))
        .pipe($.if(!PRODUCTION, $.sourcemaps.write()))
        .pipe(gulp.dest(PATHS.dist + '/js'));
}

// Copy compiled javascript files
function ml_javascript() {
    return gulp.src(PATHS.mlentries)
        .pipe(gulp.dest(PATHS.dist + '/js'));
}

const javascript =
      gulp.parallel(
          source_javascript,
          ml_javascript,
      );

// Define and register tasks

// Build the dist folder

const build =
      gulp.parallel(
          assets,
          sass,
          javascript
      );

const cleanbuild =
      gulp.series(
          clean,
          build
      );

exports.build = build;
exports.clean = clean;
exports.cleanbuild = cleanbuild;

exports.default = cleanbuild;
