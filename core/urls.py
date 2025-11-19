# core/urls.py
from django.urls import path
from . import views

urlpatterns = [
    path('', views.consulta_view, name='consulta'),
]