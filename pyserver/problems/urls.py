from django.conf.urls import patterns, url

from problems import views

urlpatterns = patterns('',
    url(r'^$', views.index, name='index'),
)
