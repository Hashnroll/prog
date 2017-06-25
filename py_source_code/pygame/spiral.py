import pygame, sys, math
from math import sin, cos, exp
from pygame.locals import *

pygame.init()

WIDTH = 1200
HEIGHT = 1000
windowSurface = pygame.display.set_mode([WIDTH, HEIGHT])
windowSurface.fill((255,255,255))

Phi = 0
x0,y0 = WIDTH/2, HEIGHT/2

#Логарифмическая спираль
while True:
    pygame.display.update()
    for event in pygame.event.get():
        if event.type == QUIT:
            pygame.quit()
            sys.exit()
        
    pixArray = pygame.PixelArray(windowSurface)
    r = exp(Phi/10)
    x, y = math.ceil(x0 + r*cos(Phi)),  math.ceil(y0 + r*sin(Phi))
    if not (x>0 and x<WIDTH and y>0 and y<HEIGHT):
        break
    pixArray[x][y] = (0,0,0)
    Phi += 0.01

Phi = 0

#Спираль Архимеда
while True:
    pygame.display.update()
    for event in pygame.event.get():
        if event.type == QUIT:
            pygame.quit()
            sys.exit()
        
    pixArray = pygame.PixelArray(windowSurface)
    r = 10*Phi
    x, y = math.ceil(x0 + r*cos(Phi)),  math.ceil(y0 + r*sin(Phi))
    if not (x>0 and x<WIDTH and y>0 and y<HEIGHT):
        break
    pixArray[x][y] = (255,0,0)
    Phi += 0.01

del pixArray
while True:
    for event in pygame.event.get():
        if event.type == QUIT:
            pygame.quit()
            sys.exit()

   


