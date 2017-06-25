import pygame, sys
from pygame.locals import *

pygame.init()

windowSurface = pygame.display.set_mode([500, 400])
pygame.display.set_caption('Hello world!')

BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
RED = (255, 0, 0)
GREEN = (0, 255, 0)
BLUE = (0, 0, 255)

basicFont = pygame.font.SysFont(None, 48)
text = basicFont.render('Hello world!', True, WHITE, BLUE)
textRect = text.get_rect()

textRect.centerx = windowSurface.get_rect().centerx
textRect.centery = windowSurface.get_rect().centery

windowSurface.fill(WHITE)

pygame.draw.polygon(windowSurface, GREEN, ((146, 0), (291, 106), (236, 277), (56, 277), (0, 106)))

pixArray = pygame.PixelArray(windowSurface)
pixArray[480][380] = BLACK
del pixArray

windowSurface.blit(text, textRect)

pygame.display.update()

for event in pygame.event.get():
    if event.type == QUIT:
        pygame.quit()
        sys.exit()



