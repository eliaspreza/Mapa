# Using `rgdal` on Heroku

Answer for the [Heroku, R and Gdal][question] question on StackOverflow, for using the [`rgdal`][rgdal] package in R on Heroku.

Using Heroku's [container stack][container-stack], together with the [heroku-docker-r][heroku-docker-r] image.

## Usage

* Build

  ```
  make build
  ```

* Test

  ```
  make run
  ```

* Deploy

  ```
  heroku create --stack=container
  git push heroku main
  ```

## Credits

Sample code from [`rgdal`][rgdal] documentation.

## License

MIT License. Copyright (c) 2021 Chris Stefano. See [LICENSE](LICENSE) for details.

<!-- links -->

[container-stack]: https://devcenter.heroku.com/categories/deploying-with-docker
[rgdal]: https://cran.r-project.org/web/packages/rgdal/index.html
[heroku-docker-r]: https://github.com/virtualstaticvoid/heroku-docker-r
[question]: https://stackoverflow.com/questions/70117665/heroku-r-and-gdal
[registry]: https://devcenter.heroku.com/articles/container-registry-and-runtime
