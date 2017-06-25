import pygame, sys, math
from pygame.locals import *

WIDTH = 800
HEIGHT = 600
UNIT = 200

Ox = math.ceil(WIDTH/2)
Oy = math.ceil(HEIGHT/2)

def mandelbrot(n = 5):
    pygame.init()
    
    windowSurface = pygame.display.set_mode([WIDTH, HEIGHT])
    pygame.display.set_caption('Mandelbrot')
    pixArray = pygame.PixelArray(windowSurface)
    windowSurface.fill((120, 120, 120))
    pygame.display.update()

    DEPTH = n
    
    for x in range(-WIDTH//2, WIDTH//2):
        for y in range(-HEIGHT//2, HEIGHT//2):       
            c = Complex()
            c.x, c.y = x / UNIT, y / UNIT
            if c.checkMandelbrot(DEPTH):
                pixArray[x + math.ceil(WIDTH/2) - 1][y + math.ceil(HEIGHT/2) - 1] = (32, 5, 63)
        drawAxes(windowSurface)
        pygame.display.update()


    while True:
        for event in pygame.event.get():
            if event.type == QUIT:
                pygame.quit()
                sys.exit()


def drawAxes(Surface):
    pygame.draw.line(Surface, (0,0,0), (Ox,0), (Ox,2*Oy))
    pygame.draw.line(Surface, (0,0,0), (0, Oy), (2*Ox,Oy))
    
class Complex:
    def __init__(self):
        self.x = 0
        self.y = 0
    def absolute(self):
        return math.sqrt(self.x*self.x + self.y*self.y)
    def sqr(self):
        x = self.x*self.x - self.y*self.y
        y = 2*self.x*self.y
        self.x = x
        self.y = y
    def checkMandelbrot(self, depth):
        z = Complex()
        for i in range(depth):
            if (z.absolute() > 2):
                return False
            for i in range(1):
                z.sqr()
            z.x, z.y = z.x + self.x, z.y + self.y
        p = z.absolute()
        print(p)
        return (p <= 2)  
