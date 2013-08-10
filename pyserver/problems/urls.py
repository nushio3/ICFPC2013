from django.conf.urls import patterns, url

from problems import views

urlpatterns = patterns('',
    url(r'^$', views.index, name='index'),
    url(r'^reload/$', views.reload, name='reload'),
    url(r'^setreload/(?P<status>True|False)/$', views.setreload,
        name='setreload'),
    url(r'^maybereload/$', views.maybereload, name='maybereload')
)
