#!/usr/bin/env python

import game
import numpy
import pygame
import time
import sys

fullscreen = False

if fullscreen:
    desired_size = None
else:
    desired_size = (800,600)

pygame.init()
pygame.display.set_caption('Diving Beet')
if desired_size != None:
    screen = pygame.display.set_mode(desired_size)
else:
    screen = pygame.display.set_mode()
width = screen.get_width()
height = screen.get_height()
size = (width,height)
font = pygame.font.Font(None, 36)
pygame.key.set_repeat(100, 100)

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)


beet = game.game()
def test_assoc(beet):
    e0 = (1,False, False)
    ve = (0,True, False)
    e1 = (3,False, False)

    e2 = (4,False, False)

    elms0 = [e0,ve]
    elms1 = [e1,ve]
    elms2 = [e2,ve]
    for e0 in elms0:
        for e1 in elms1:
            for e2 in elms2:
                pres00 = beet.test_assoc_pres(e0[0], e0[1], e0[2], e1[0],e1[1],e1[2])
                pres01 = beet.test_assoc_pres(pres00[0], pres00[1], pres00[2], e2[0],e2[1], e2[2])
                pres10 = beet.test_assoc_pres(e1[0],e1[1], e1[2], e2[0],e2[1], e2[2])
                pres11 = beet.test_assoc_pres(e0[0],e0[1], e0[2], pres10[0],pres10[1], pres10[2])

                if not pres01[0] == pres11[0]:
                    print e0,e1,e2
                    print pres01
                    print pres11

test_assoc(beet)
#exit()
beet_state = beet.new_game(width, height)

# Initialise the list of insertable elements and their names.
insertable = []
for elem in beet.insertable_elements().get():
    name = ''.join(map(chr,list(beet.element_name(elem).get())))
    insertable += [(name, elem)]
insertable.sort()
num_insertable = len(insertable)

selection = 0
# Set the initial selection to sand.
for i in range(num_insertable):
    if insertable[i][0] == 'sand':
        selection = i
        break

modify_radius = 5

while True:
    start = time.time()
    beet_state = beet.step_game(*beet_state)

    frame = beet.render(*beet_state).get()
    end = time.time()
    futhark_time = (end-start)*1000

    start = time.time()
    pygame.surfarray.blit_array(screen, frame)
    end = time.time()
    blit_time = (end-start)*1000

    speedmessage = "Futhark call took %.2fms; blitting %.2fms" % \
                   (futhark_time, blit_time)
    showText(speedmessage, (10, 10))

    showText('%s (radius %d)' % (insertable[selection][0], modify_radius), (10,40))

    pygame.display.flip()

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.MOUSEBUTTONDOWN:
            if event.button == 4:
                modify_radius = min(modify_radius + 1, 100)
            if event.button == 5:
                modify_radius = max(modify_radius - 1, 1)
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_RIGHT:
                selection = (selection + 1) % num_insertable
            if event.key == pygame.K_LEFT:
                selection = (selection - 1) % num_insertable

    if pygame.mouse.get_pressed()[0] and pygame.mouse.get_pos() != None:
        # insert the selected element here.
        args = beet_state + pygame.mouse.get_pos() + (modify_radius, insertable[selection][1])
        beet_state = beet.add_element(*args)
    if pygame.mouse.get_pressed()[2] and pygame.mouse.get_pos() != None:
        # remove any element element here.
        args = beet_state + pygame.mouse.get_pos() + (modify_radius,)
        beet_state = beet.clear_element(*args)
